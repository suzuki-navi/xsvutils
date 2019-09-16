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

