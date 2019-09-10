// mulang-bin-sources: scala

import scala.concurrent.Future;

trait ProcessBuildingTask {

  def messages: List[String];

  //def start(): Future[Unit];

}

case class MkfifoProcessBuildingTask (
  path: WorkingFilePath,
) extends ProcessBuildingTask {

def messages: List[String] =
    ("$ mkfifo " + path.debugStr) ::
    Nil;

}

case class ForkProcessBuildingTask (
  command: List[Either[String, FilePath]],
  input: Option[FilePath],
  output: Option[FilePath],
) extends ProcessBuildingTask {

def messages: List[String] = {
    val commandStr = command.map {
      case Left(s) => s;
      case Right(p) => p.debugStr;
    }.mkString(" ");
    ("$ " + commandStr + " \\") ::
    ("  < " + input.map(_.debugStr).getOrElse("/dev/null")) ::
    ("  > " + output.map(_.debugStr).getOrElse("/dev/null")) ::
    "" ::
    Nil;
  }

}

trait FilePath {
  def path: String;
  def debugStr: String;
}
case class UserFilePath (
  path: String,
) extends FilePath {
  def debugStr: String = path;
}
case class WorkingFilePath (
  name: String,
) extends FilePath {
  def path: String = "./tmp/" + name; // TODO
  def debugStr: String = "./tmp/" + name; // TODO
}
case class UserPipePath (
  origPath: String,
  pipeName: String,
  compressionType: List[CompressionType],
) extends FilePath {
  def path: String = "./tmp/" + pipeName; // TODO
  def debugStr: String = "./tmp/" + pipeName + // TODO
    "[" + origPath + "|" + compressionType.map(_.name).mkString("|") + "]";
}

object ProcessSeqBuilder {

  def build(inputs: IndexedSeq[Graph.Node[CommandGraphNode]]): IndexedSeq[ProcessBuildingTask] = {

    type Node = Graph.Node[CommandGraphNode];
    type Edge = Graph.Edge[CommandGraphNode];

    @scala.annotation.tailrec
    def sub(inputs: IndexedSeq[Node], edges: IndexedSeq[Edge],
      tasks: IndexedSeq[ProcessBuildingTask]): IndexedSeq[ProcessBuildingTask] = {
      inputs.find { node =>
        val a = inputs.count(_ == node);
        node.prevCount <= a;
      } match {
        case Some(node) =>
          val outputEdges = node.nexts;
          val edges2 = edges ++ outputEdges;
          val tasks2 = createNodeTasks(node, edges2, edges.size);
          val inputs2 = outputEdges.map(e => e.next) ++ inputs.filter(_ != node);
          sub(inputs2, edges2, tasks ++ tasks2);
        case None =>
          if (inputs.nonEmpty) {
            throw new AssertionError();
          }
          tasks;
      }
    }

    def createNodeTasks(node: Node, edges: IndexedSeq[Edge], offset: Int):
      IndexedSeq[ProcessBuildingTask] = {
      val inputs: IndexedSeq[WorkingFilePath] = (0 until node.prevCount).map { j =>
        val edgeIndex = edges.indexWhere(e => e.next == node && e.id == j);
        if (edgeIndex < 0) {
          throw new AssertionError();
        }
        //println("# < %d".format(edgeIndex));
        workingFilePath(edgeIndex);
      }
      val outputs: IndexedSeq[WorkingFilePath] = (offset until edges.size).map { edgeIndex =>
        //val edge = edges(i);
        //println("# > %d".format(edgeIndex));
        workingFilePath(edgeIndex);
      }
      //pprint.pprintln(node.payload);
      //println("");
      val mkfifos = outputs.map(MkfifoProcessBuildingTask(_));
      val task = node.payload.toTask(inputs, outputs);
      mkfifos :+ task;
    }

    def workingFilePath(edgeIndex: Int): WorkingFilePath = {
      WorkingFilePath("pipe-%d.tsv".format(edgeIndex + 1));
    }

    val inputs2: IndexedSeq[Graph.Node[CommandGraphNode]] = CommandGraph.toProcessNode(inputs);
    sub(inputs2, Vector.empty, Vector.empty);

  }

}

