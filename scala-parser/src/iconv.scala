// mulang-bin-sources: scala

case class IconvCommandGraphNode (
  charencoding: String, // UTF-8, cp932
) extends CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

}

