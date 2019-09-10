// mulang-bin-sources: scala

import scala.concurrent.Await;
import scala.concurrent.Future;
import scala.concurrent.duration.Duration;

object CommandGraph {

  def unshift(output: FileOutputCommandGraphNode):
    Graph.Edge[CommandGraphNode] = {
    Graph.unshiftEdges(output, Vector.empty, 1)(0);
  }

  def unshift(input: FileInputCommandGraphNode, edge: Graph.Edge[CommandGraphNode]):
    Graph.Node[CommandGraphNode] = {
    Graph.unshiftNode(input, Vector(edge));
  }

  def unshiftCommands(commands: IndexedSeq[CommandPipeNode], edge: Graph.Edge[CommandGraphNode]):
    (Graph.Edge[CommandGraphNode], IndexedSeq[Graph.Node[CommandGraphNode]]) = {
    commands.reverse.foldLeft[(Graph.Edge[CommandGraphNode], IndexedSeq[Graph.Node[CommandGraphNode]])](
      (edge, Vector.empty)) { (t, cmd) =>
      val (edge, nodes) = t;
      val (edge2, nodes2) = cmd.addNodeToGraph(edge);
      (edge2, nodes2 ++ nodes);
    }
  }

  def toProcessNode(inputs: IndexedSeq[Graph.Node[CommandGraphNode]]):
    IndexedSeq[Graph.Node[CommandGraphNode]] = {

    type Node = Graph.Node[CommandGraphNode];
    type Edge = Graph.Edge[CommandGraphNode];
    val Edge = Graph.Edge;

    // Scalaでは末尾呼び出し最適化がされず、コマンドの長さに比例したスタックを掘ってしまう
    def convertNodes(nodes: IndexedSeq[Node]): IndexedSeq[Node] = {
      var nodeMap: Map[Node, Node] = Map.empty;
      var edgeMap: Map[Edge, Edge] = Map.empty;
      nodes.map { node =>
        val (node2, nodeMap2, edgeMap2) = convertNode(node, nodeMap, edgeMap);
        nodeMap = nodeMap2;
        edgeMap = edgeMap2;
        node2;
      }
    }

    def convertNode(node: Node, nodeMap: Map[Node, Node], edgeMap: Map[Edge, Edge]): (Node, Map[Node, Node], Map[Edge, Edge]) = {
      var nodeMap2: Map[Node, Node] = nodeMap;
      var edgeMap2: Map[Edge, Edge] = edgeMap;
      val newNexts = node.nexts.map { edge =>
        val (edge2, nodeMap3, edgeMap3) = convertEdge(edge, nodeMap2, edgeMap2);
        nodeMap2 = nodeMap3;
        edgeMap2 = edgeMap3;
        edge2;
      }
      val newNode = node.payload.toProcessNode(node, newNexts);
      (newNode, nodeMap2 + (node -> newNode), edgeMap2);
    }

    def convertEdge(edge: Edge, nodeMap: Map[Node, Node], edgeMap: Map[Edge, Edge]): (Edge, Map[Node, Node], Map[Edge, Edge]) = {
      var nodeMap2: Map[Node, Node] = nodeMap;
      var edgeMap2: Map[Edge, Edge] = edgeMap;
      val newNext = {
        val (node2, nodeMap3, edgeMap3) = convertNode(edge.next, nodeMap2, edgeMap2);
        nodeMap2 = nodeMap3;
        edgeMap2 = edgeMap3;
        node2;
      }
      val newEdge = if (newNext == edge.next) {
        edge;
      } else {
        Edge(newNext, edge.id)
      }
      (newEdge, nodeMap2, edgeMap2 + (edge -> newEdge));
    }

    convertNodes(inputs);
  }

}

trait CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode];

  protected[this] def toProcessNodeDefault(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] = {
    if (node.nexts == newNexts) {
      node;
    } else {
      Graph.Node(node.payload, newNexts, node.prevCount);
    }
  }

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask;

}

case class FileInputCommandGraphNode (
  tableFormat: Option[InputTableFormat],
  path: String, // 空文字列は標準入力の意味
) extends CommandGraphNode {

  private[this] val formatReaderFuture: Future[FormatReader.Result] = {
    FileInputCommandGraphNode.fileInputCount = FileInputCommandGraphNode.fileInputCount + 1;
    FormatReader.read(this, FileInputCommandGraphNode.fileInputCount);
  }

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] = {
    val newNext = newNexts(0);
    val result: FormatReader.Result = Await.result(formatReaderFuture, Duration.Inf);
    val nodes = FileInputCommandGraphNode.toCommandGraphNodeSeq(result);
    val newEdge = nodes.tail.reverse.foldLeft[Graph.Edge[CommandGraphNode]](newNext) { (edge, node) =>
      val newNode = Graph.Node[CommandGraphNode](node, Vector(edge), 1);
      val newEdge = Graph.Edge[CommandGraphNode](newNode, 0);
      newEdge;
    }
    val newNode = Graph.Node[CommandGraphNode](nodes.head, Vector(newEdge), 0);
    newNode;
  }

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    throw new AssertionError(); // ここにはこないはず
  }

}

object FileInputCommandGraphNode {

  private var fileInputCount: Int = 0;

  private def toCommandGraphNodeSeq(result: FormatReader.Result):
    List[CommandGraphNode] = {
    val newlineTypeNodes = Nil; // TODO
    val tableFormatNodes = Nil; // TODO
    val charencodingNodes = result.charencoding match {
      case Charencoding.Utf8 =>
        Nil;
      case Charencoding.Utf8bom =>
        BomTailCommandGraphNode() :: Nil;
      case Charencoding.Sjis =>
        IconvCommandGraphNode("cp932") :: Nil;
    }
    RawFileInputCommandGraphNode(result) ::
      newlineTypeNodes ::: tableFormatNodes ::: charencodingNodes;
  }

}


case class RawFileInputCommandGraphNode (
  formatResult: FormatReader.Result,
) extends CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    val input = formatResult.path;
    ForkProcessBuildingTask(Left("cat") :: Nil, Some(input), Some(outputs(0)));
  }

}

case class FileOutputCommandGraphNode (
  path: String, // 空文字列は標準出力の意味
) extends CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    val output = UserFilePath(path); // TODO 空文字列の対応
    ForkProcessBuildingTask(Left("cat") :: Nil, Some(inputs(0)), Some(output));
  }

}

