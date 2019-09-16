// mulang-bin-sources: main-jvm

case class CatCommandGraphNode (
  input: Option[FilePath],
  output: Option[FilePath],
) extends CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    val input2 = input.getOrElse(inputs(0));
    val output2 = output.getOrElse(outputs(0));
    ForkProcessBuildingTask(this, Left("cat") :: Nil, input2, output2);
  }

}

