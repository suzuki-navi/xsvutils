// mulang-bin-sources: main-jvm

case class ToDiffableCommandGraphNode (
) extends CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    ForkProcessBuildingTask(this, Left("perl") :: Right(SourceFilePath("to-diffable.pl")) :: Nil, inputs(0), outputs(0));
  }

}

