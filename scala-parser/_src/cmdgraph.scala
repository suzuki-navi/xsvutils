// mulang-bin-sources: scala

// コマンドの接続を有向グラフにたとえて表現する
case class CommandGraph (
  edgeCount: Int,
  nodes: Vector[CommandGraphNode],
) {

  def addNode(command: CommandGraphNode): CommandGraph = {
    this.copy(nodes = nodes :+ command);
  }

  def addCommandSeq(commands: Vector[CommandSeqNode],
    inputEdgeId: Int, outputEdgeId: Int): CommandGraph = {
    commands.indexWhere(_.isCommandGraphNode) match {
      case p if p < 0 =>
        val seq = CommandGraph.CommandGraphNodeSeq(commands, inputEdgeId, outputEdgeId);
        this.addNode(seq);
      case p =>
        val prevCommands = commands.slice(0, p);
        val nextCommands = commands.slice(p + 1, commands.size);
        commands(p).addNodeToGraph(this, prevCommands, nextCommands, inputEdgeId, outputEdgeId);
    }
  }

  def addEdge: (CommandGraph, Int) = {
    val edgeId = edgeCount;
    (this.copy(edgeCount = edgeCount + 1), edgeId);
  }

}

object CommandGraph {

  def init: CommandGraph = CommandGraph(0, Vector.empty);

  private case class CommandGraphNodeSeq (
    commands: Vector[CommandSeqNode],
    inputEdgeId: Int,
    outputEdgeId: Int,
  ) extends CommandGraphNode {
    def inputs:  List[Int] = inputEdgeId :: Nil;
    def outputs: List[Int] = outputEdgeId :: Nil;
  }

}

// 頂点(ノード) = 複雑なコマンド(diff等)や入出力
// コマンドは0個以上の入力となるエッジと0個以上の出力となるエッジが接続する
trait CommandGraphNode {
  def inputs:  List[Int]; // コマンドにとっての入力
  def outputs: List[Int]; // コマンドにとっての出力
}

case class FileInputCommandGraphNode (
  inputFormat: Option[InputFormat],
  inputFile: String, // 空文字列は標準入力の意味
  edgeId: Int,
) extends CommandGraphNode {
  def inputs:  List[Int] = Nil;
  def outputs: List[Int] = edgeId :: Nil;
}

case class FileOutputCommandGraphNode (
  outputFile: String, // 空文字列は標準出力の意味
  edgeId: Int,
) extends CommandGraphNode {
  def inputs:  List[Int] = edgeId :: Nil;
  def outputs: List[Int] = Nil;
}

