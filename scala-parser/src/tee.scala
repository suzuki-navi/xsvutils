// mulang-bin-sources: scala

case class TeeCommandGraphNode (
  inputEdgeId: Int,
  outputEdgeIds: List[Int],
) extends CommandGraphNode {
  def inputs:  List[Int] = inputEdgeId :: Nil;
  def outputs: List[Int] = outputEdgeIds;
}

