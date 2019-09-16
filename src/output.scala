// mulang-bin-sources: main-jvm

import scala.concurrent.Await;
import scala.concurrent.Future;
import scala.concurrent.duration.Duration;

case class FileOutputCommandGraphNode (
  path: String, // 空文字列は標準出力の意味
) extends CommandGraphNode {

  //private[this] val fileTypeFuture: Future[Boolean] = {
  //  implicit val ec = ProcessUtil.executorContext;
  //  Future[Boolean] {
  //    if (path.isEmpty) {
  //      false;
  //    } else {
  //      OutputFileType.isOutputRegularFile(path);
  //    }
  //  }
  //}

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] = {
    val nodes = FileOutputCommandGraphNode.toCommandGraphNodeSeq(path);
    val nodesReversed = nodes.reverse;
    val rawOutputNode = Graph.Node[CommandGraphNode](nodesReversed.head, Vector.empty, 1);
    nodesReversed.tail.foldLeft[Graph.Node[CommandGraphNode]](rawOutputNode) { (graphNode, cmdNode) =>
      val edge = Graph.Edge[CommandGraphNode](graphNode, 0);
      Graph.Node[CommandGraphNode](cmdNode, Vector(edge), 1);
    }
  }

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    throw new AssertionError(); // ここにはこないはず
  }

}

object FileOutputCommandGraphNode {

  private def toCommandGraphNodeSeq(path: String):
    List[CommandGraphNode] = {
    val path2 = if (path.isEmpty) {
      StdoutPipePath;
    } else {
      UserFilePath(path);
    }
    if (path.isEmpty && Main.isOutputTty) {
      CatCommandGraphNode(None, Some(path2)) :: Nil; // TODO
    } else {
      CatCommandGraphNode(None, Some(path2)) :: Nil;
    }
  }

}

object OutputFileType {

  //def isOutputRegularFile(path: String): Boolean = {
  //  val r = ProcessUtil.doProcessExitValue(List("test", "-f", path),
  //    ProcessUtil.NullInput, ProcessUtil.NullOutput);
  //  r == 0;
  //}

}

