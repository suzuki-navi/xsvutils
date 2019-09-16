// mulang-bin-sources: scala

import java.io.IOException;
import scala.concurrent.Await;
import scala.concurrent.Future;
import scala.concurrent.duration.Duration;

trait ProcessBuildingTask {

  def nodeMessages: List[String];

  def formatMessages: List[String];

  def taskMessages: List[String];

  def start(): Future[Unit];

}

case class ForkProcessBuildingTask (
  node: CommandGraphNode,
  command: List[Either[String, FilePath]],
  input: FilePath,
  output: FilePath,
) extends ProcessBuildingTask {

  def nodeMessages: List[String] = {
    val nodeInfo: String = {
      pprint.tokenize(node, width = 100).map { _.plainText }.mkString("");
    }
    nodeInfo.split("\n").map("# " + _).toList.filter(!_.isEmpty);
  }

  def formatMessages: List[String] = {
    val files: List[FilePath] = input :: output :: command.flatMap {
      case Left(_) => Nil;
      case Right(path) => path :: Nil;
    }
    files.flatMap(_.taskMessages);
  }

  def taskMessages: List[String] = {
    val commandStr = command.map {
      case Left(s) => s;
      case Right(p) => p.debugStr;
    }.mkString(" ");
    ("$ " + commandStr + " \\") ::
    ("  < " + input.debugStr + " \\") ::
    ("  > " + output.debugStr + " &") ::
    Nil;
  }

  def start(): Future[Unit] = {
    implicit val ec = ProcessUtil.executorContext;
    val cmd: List[String] = command.map {
      case Left(s) => s;
      case Right(p) => p.path;
    }
    val future = Future[Unit] {
      val i = input match {
        case NullPath => ProcessUtil.NullInput;
        case _ => ProcessUtil.FileInput(input.path);
      }
      val o = output match {
        case StdoutPipePath => ProcessUtil.Stdout;
        case NullPath => ProcessUtil.NullOutput;
        case _ => ProcessUtil.FileOutput(output.path);
      }
      try {
        ProcessUtil.doProcess(cmd, i, o);
      } catch {
        case e: IOException =>
          System.err.println(e.getMessage);
      }
    }
    future;
  }

}

case class ProcessBuilder (tasks: IndexedSeq[(List[MkfifoInfo], ProcessBuildingTask)]) {

  def explain() {
    tasks.foreach { case (mkfifos, task) =>
      task.nodeMessages.foreach { line =>
        println(line);
      }
      task.formatMessages.foreach { line =>
        println(line);
      }
      mkfifos.foreach { mkfifo =>
        println(mkfifo.message);
      }
      task.taskMessages.foreach { line =>
        println(line);
      }
      println();
    }
  }

  def start(): Future[Unit] = {
    implicit val ec = ProcessUtil.executorContext;
    var futures: IndexedSeq[Future[Unit]] = Vector.empty;
    tasks.foreach { case (mkfifos, task) =>
      mkfifos.foreach { mkfifo =>
        mkfifo.run();
      }
      val f = task.start();
      futures = futures :+ f;
    }
    Future[Unit] {
      futures.foreach { f =>
        Await.ready(f, Duration.Inf);
      }
    }
  }

}

object ProcessBuilder {

  def build(inputs: IndexedSeq[Graph.Node[CommandGraphNode]]): ProcessBuilder = {

    type Node = Graph.Node[CommandGraphNode];
    type Edge = Graph.Edge[CommandGraphNode];

    @scala.annotation.tailrec
    def sub(inputs: IndexedSeq[Node], edges: IndexedSeq[Edge],
      tasks: IndexedSeq[(List[MkfifoInfo], ProcessBuildingTask)]): ProcessBuilder = {
      inputs.find { node =>
        val a = inputs.count(_ == node);
        node.prevCount <= a;
      } match {
        case Some(node) =>
          val outputEdges = node.nexts;
          val edges2 = edges ++ outputEdges;
          val task2 = createNodeTasks(node, edges2, edges.size);
          val inputs2 = outputEdges.map(e => e.next) ++ inputs.filter(_ != node);
          sub(inputs2, edges2, tasks :+ task2);
        case None =>
          if (inputs.nonEmpty) {
            throw new AssertionError();
          }
          ProcessBuilder(tasks);
      }
    }

    def createNodeTasks(node: Node, edges: IndexedSeq[Edge], offset: Int):
      (List[MkfifoInfo], ProcessBuildingTask) = {
      val inputs: IndexedSeq[WorkingFilePath] = (0 until node.prevCount).map { j =>
        val edgeIndex = edges.indexWhere(e => e.next == node && e.id == j);
        if (edgeIndex < 0) {
          throw new AssertionError();
        }
        workingFilePath(edgeIndex);
      }
      val outputs: IndexedSeq[WorkingFilePath] = (offset until edges.size).map { edgeIndex =>
        //val edge = edges(i);
        workingFilePath(edgeIndex);
      }
      val mkfifos = outputs.toList.map(MkfifoInfo(_));
      val task = node.payload.toTask(inputs, outputs);
      (mkfifos, task);
    }

    def workingFilePath(edgeIndex: Int): WorkingFilePath = {
      WorkingFilePath("pipe-%d.tsv".format(edgeIndex + 1));
    }

    val inputs2: IndexedSeq[Graph.Node[CommandGraphNode]] = CommandGraph.toProcessNode(inputs);
    sub(inputs2, Vector.empty, Vector.empty);

  }

}

