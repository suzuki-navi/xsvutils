// mulang-bin-sources: scala

case class IconvCommandGraphNode (
  charencoding: String, // UTF-8, cp932
) extends CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    ForkProcessBuildingTask(this, Left("iconv") :: Left("-f") :: Left(charencoding) ::
      Left("-t") :: Left("UTF-8//TRANSLIT") :: Nil,
      inputs(0), outputs(0));
  }

}

