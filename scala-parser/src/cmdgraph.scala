// mulang-bin-sources: scala

// コマンドの接続を有向グラフにたとえて表現する
object CommandGraph {

  case class Commands (nodes: Vector[Node], edges: Vector[Edge]);

  // 頂点(ノード) = コマンド
  // コマンドは0個以上の入力となるエッジと0個以上の出力となるエッジが接続する
  case class Node ();

  // 辺(エッジ) = コマンドとコマンドをつなぐFIFO
  case class Edge ();

  def toCommandGraph(commands: CommandSeq): Unit = {
  }

}

