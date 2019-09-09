// mulang-bin-sources: scala

case class BomTailCommandGraphNode (
) extends CommandGraphNode {
  // tail -c+4

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

}

