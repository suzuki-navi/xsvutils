// mulang-bin-sources: scala

// コマンドの接続を有向グラフにたとえて表現する
case class CommandGraph (
  edgeCount: Int,
  nodes: Vector[CommandGraphNode],
) {

  def addEdge: (CommandGraph, Int) = {
    val edgeId = edgeCount;
    (this.copy(edgeCount = edgeCount + 1), edgeId);
  }

  def addNode(command: CommandGraphNode): CommandGraph = {
    this.copy(nodes = nodes :+ command);
  }

  def addCommandSeq(commands: Vector[CommandNode], inputEdgeId: Int): (CommandGraph, Int) = {
    if (commands.isEmpty) {
      (this, inputEdgeId);
    } else {
      commands.indexWhere(_.isCommandGraphNode) match {
        case p if p < 0 =>
          val  graph0 = this;
          val (graph1, outputEdgeId) = graph0.addEdge;
          val seq = CommandGraph.NodeSeqCommandGraph(commands, inputEdgeId, outputEdgeId);
          val  graph2 = graph1.addNode(seq);
          (graph2, outputEdgeId);
        case p =>
          val prevCommands = commands.slice(0, p);
          val nextCommands = commands.slice(p + 1, commands.size);
          commands(p).addNodeToGraph(this, prevCommands, nextCommands, inputEdgeId);
      }
    }
  }

  def addFileInput(inputFormat: Option[InputTableFormat], inputFile: String): (CommandGraph, Int) = {
    val  graph0 = this;
    val (graph1, edgeId) = graph0.addEdge;
    val  graph2 = graph1.addNode(CommandGraph.FileInputCommandGraphNode(inputFormat, inputFile, edgeId));
    (graph2, edgeId);
  }

  def addFileOutput(outputFile: String, edgeId: Int): CommandGraph = {
    val  graph0 = this;
    val  graph1 = graph0.addNode(CommandGraph.FileOutputCommandGraphNode(outputFile, edgeId));
    graph1;
  }

  def fileInputList: Vector[CommandGraph.FileInputCommandGraphNode] = {
    nodes.flatMap {
      case cmd: CommandGraph.FileInputCommandGraphNode => cmd :: Nil;
      case _ => Nil;
    }
  }

}

object CommandGraph {

  def init: CommandGraph = CommandGraph(0, Vector.empty);

  private case class NodeSeqCommandGraph (
    commands: Vector[CommandNode],
    inputEdgeId: Int,
    outputEdgeId: Int,
  ) extends CommandGraphNode {
    assert(!commands.exists(_.isCommandGraphNode));
    def inputs:  List[Int] = inputEdgeId :: Nil;
    def outputs: List[Int] = outputEdgeId :: Nil;
  }

  case class FileInputCommandGraphNode private (
    format: Option[InputTableFormat],
    path: String, // 空文字列は標準入力の意味
    edgeId: Int,
  ) extends CommandGraphNode {
    def inputs:  List[Int] = Nil;
    def outputs: List[Int] = edgeId :: Nil;
  }

  case class FileOutputCommandGraphNode private (
    path: String, // 空文字列は標準出力の意味
    edgeId: Int,
  ) extends CommandGraphNode {
    def inputs:  List[Int] = edgeId :: Nil;
    def outputs: List[Int] = Nil;
  }

}

// 頂点(ノード) = 複雑なコマンド(diff等)や入出力
// コマンドは0個以上の入力となるエッジと0個以上の出力となるエッジが接続する
trait CommandGraphNode {
  def inputs:  List[Int]; // コマンドにとっての入力
  def outputs: List[Int]; // コマンドにとっての出力
}


