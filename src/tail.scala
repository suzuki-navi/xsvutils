// mulang-bin-sources: main-jvm

case class BomTailCommandGraphNode (
) extends CommandGraphNode {
  // tail -c+4

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    ForkProcessBuildingTask(this, Left("tail") :: Left("-c+4") :: Nil,
      inputs(0), outputs(0));
  }

}

