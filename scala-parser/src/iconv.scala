// mulang-bin-sources: scala

case class IconvCommandGraphNode (
  charencoding: String, // UTF-8, cp932
) extends CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    ForkProcessBuildingTask(Left("iconv") :: Left("-f") :: Left(charencoding) :: Nil,
      Some(inputs(0)), Some(outputs(0)));
  }

}

