// mulang-bin-sources: scala

object CommandGraph {

  def unshift(output: FileOutputCommandNode):
    Graph.Edge[CommandNode] = {
    Graph.unshiftEdges(output, Vector.empty, 1)(0);
  }

  def unshift(input: FileInputCommandNode, edge: Graph.Edge[CommandNode]):
    Graph.Node[CommandNode] = {
    Graph.unshiftNode(input, Vector(edge));
  }

  def unshiftCommands(commands: IndexedSeq[CommandPipeNode], edge: Graph.Edge[CommandNode]):
    (Graph.Edge[CommandNode], IndexedSeq[Graph.Node[CommandNode]]) = {
    commands.reverse.foldLeft[(Graph.Edge[CommandNode], IndexedSeq[Graph.Node[CommandNode]])](
      (edge, Vector.empty)) { (t, cmd) =>
      val (edge, nodes) = t;
      val (edge2, nodes2) = cmd.addNodeToGraph(edge);
      (edge2, nodes2 ++ nodes);
    }
  }

}

case class FileInputCommandNode private (
  format: Option[InputTableFormat],
  path: String, // 空文字列は標準入力の意味
) extends CommandNode {

  //val formatReaderFuture: Future[FormatReader.Result] = {
  //  fileInputCount = fileInputCount + 1;
  //  FormatReader.read(this, fileInputCount);
  //}
}

case class FileOutputCommandNode (
  path: String, // 空文字列は標準出力の意味
) extends CommandNode {

}

