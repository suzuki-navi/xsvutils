// mulang-bin-sources: scala

case class TeeCommandGraphNode (
) extends CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    ForkProcessBuildingTask(Left("tee") :: Right(outputs(1)) :: Nil, Some(inputs(0)), Some(outputs(0)));
  }

}

