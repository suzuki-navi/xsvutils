// mulang-bin-sources: scala

case class CommandSeq (
  inputFormat: Option[InputFormat],
  inputFile: Option[String], // 空文字列は標準入力の意味、Noneはjoinなどの入力を引き継ぐ意味
  outputFile: Option[String], // 空文字列は標準出力の意味、Noneはjoinなどへの出力の意味
  commands: Vector[CommandSeqNode]) {

  def toTreeString: List[String] = {
    "inputFormat: %s".format(inputFormat.map(_.name).getOrElse("auto")) ::
    "inputFile: %s".format(inputFile.getOrElse("->")) ::
    "outputFile: %s".format(outputFile.getOrElse("->")) ::
    "commands: " ::
    (commands.toList.flatMap { cmd =>
      cmd.toTreeString match {
        case h :: t => "  - %s".format(h) :: t.map("    " + _);
        case Nil => "  - " :: Nil;
      }
    });
  }

  def toCommandGraph: CommandGraph = {
    throw new AssertionError("TODO");
  }

}

object CommandSeq {

  def inputFile(file: String) = CommandSeq (
    inputFormat = None,
    inputFile = Some(file),
    outputFile = None,
    commands = Vector.empty,
  );

  def apply(
    inputFormat: Option[InputFormat],
    inputFile: Option[String],
    outputFile: Option[String],
    lastCommand: OptionParser.CommandOptions,
  ): CommandSeq = {
    @scala.annotation.tailrec
    def sub(cmd: OptionParser.CommandOptions, seq: List[CommandSeqNode]): CommandSeq = {
      cmd match {
        case OptionParser.NoneCommandOptions =>
          CommandSeq(
            inputFormat,
            inputFile,
            outputFile,
            seq.toVector);
        case cmd: OptionParser.SomeCommandOptions =>
          sub(cmd.prevCommand, cmd.command :: seq);
      }
    }
    sub(lastCommand, Nil);
  }

}

trait CommandSeqNode {

  // 出力がTSV形式かどうか
  // TSV形式でない場合はこの後ろに次のコマンドを配置することができない
  def isOutputTsv: Boolean;

  // CommandGraphにてノードとして扱うかどうか
  def isCommandGraphNode: Boolean;

  // CommandGraphにてノードとして扱う場合に前後のコマンド列を含めてエッジとして追加する
  def addNodeToGraph(graph: CommandGraph,
    prevCommands: Vector[CommandSeqNode], nextCommands: Vector[CommandSeqNode],
    inputEdgeId: Int, outputEdgeId: Int): CommandGraph;

  def toTreeString: List[String];

}

sealed trait InputFormat { def name: String }
case object TsvInputFormat extends InputFormat { def name = "tsv" }
case object CsvInputFormat extends InputFormat { def name = "csv" }

