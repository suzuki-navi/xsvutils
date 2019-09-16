// mulang-bin-sources: main-jvm

case class TableCommandGraphNode (
  columnNumber: Boolean,
  recordNumber: Boolean,
  maxWith: Option[Int],
  isColor: Boolean,
) extends CommandGraphNode {

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    val cmd =
      Left("perl") :: Right(SourceFilePath("table.pl")) ::
      (if (columnNumber) {
        Left("--col-number") :: Nil;
      } else {
        Nil;
      }) :::
      (if (recordNumber) {
        Left("--record-number") :: Nil;
      } else {
        Nil;
      }) :::
      (maxWith match {
        case Some(w) => Left("--max-width") :: Left(w.toString) :: Nil;
        case None => Nil;
      }) :::
      (if (isColor) {
        Left("--color") :: Nil;
      } else {
        Nil;
      }) :::
      Nil;
    ForkProcessBuildingTask(this, cmd, inputs(0), outputs(0));
  }

}

