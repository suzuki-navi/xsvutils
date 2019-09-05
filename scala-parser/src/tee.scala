// mulang-bin-sources: scala

case class TeeCommandGraphNode(
  input: Int,
  output1: Int,
  output2: Int,
) extends CommandGraphNode {
  def inputs:  List[Int] = input :: Nil;
  def outputs: List[Int] = output1 :: output2 :: Nil;
}

