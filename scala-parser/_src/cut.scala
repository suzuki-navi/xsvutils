// mulang-bin-sources: scala

import OptionParser.ForCommand._;

object CutCommandParser extends CommandParser {
  def initStatus(lastCommand: CommandOptions, argIdx: Int): CommandParserStatus =
    CutCommandParserStatus(lastCommand, argIdx, None);
}

case class CutCommandParserStatus (
  prevCommand: CommandOptions,
  argIdx: Int,
  cols: Option[List[String]],
) extends CommandParserStatus {

  def parseOption(status: CommandSeqParserStatus,
    opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Option[(Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)])] = {
    if (opt == "--cols") {
      if (cols.isEmpty) {
        tail match {
          case arg2 :: tail2 =>
            Some((Right(status.copy(lastCommand = this.copy(cols = Some(arg2.split(",").toList)))),
              None, Some((tail2, argIdx + 2))));
          case Nil =>
            Some((Left(ParserErrorMessage(argIdx, "cols expected")),
              Some(completionCols), Some((tail, argIdx + 1))));
        }
      } else {
        Some((Left(ParserErrorMessage(argIdx, "duplicated option")), None, None));
      }
    } else {
      None;
    }
  }

  def parseArgument(status: CommandSeqParserStatus,
    arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]) = {
    if (cols.isEmpty) {
      (Right(status.copy(lastCommand = this.copy(cols = Some(arg.split(",").toList)))), None, Some((tail, argIdx + 1)));
    } else {
      (Left(ParserErrorMessage(argIdx, "unknown argument")), None, None);
    }
  }

  def help = new HelpDocument {
    def document: List[String] = List(
      "TODO",
    );
  }

  def completion = new Completion {
    def isFilePath: Boolean = false;
    def options: List[String] = Nil; // TODO
    def commandsEnable: Boolean = false; // TODO
  }

  private[this] def completionCols = new Completion {
    def isFilePath: Boolean = false;
    def options: List[String] = Nil;
    def commandsEnable: Boolean = false;
  }

  def finish(status: CommandSeqParserStatus):
    Either[CommandSeqReceiver, Either[ParserErrorMessage, CommandOptions]] = {
    cols match {
      case None =>
        Right(Left(ParserErrorMessage(argIdx, "expected --cols option")));
      case Some(cols) =>
        Right(Right(SomeCommandOptions(prevCommand, CutCommandSeqNode(cols))));
    }
  }

}

case class CutCommandSeqNode (
  cols: List[String],
) extends CommandSeqNode {

  // コマンド実装時向けのコメント
  // 出力がTSV形式かどうか。
  // TSV形式で出力するコマンドでは true、
  // そうでない場合は falseとする。
  def isOutputTsv: Boolean = true;

  // コマンド実装時向けのコメント
  // CommandGraphにてノードとして扱うかどうか・
  // シンプルな入出力のコマンドでは false
  def isCommandGraphNode: Boolean = false;

  def addNodeToGraph(graph: CommandGraph,
    prevCommands: Vector[CommandSeqNode], nextCommands: Vector[CommandSeqNode],
    inputEdgeId: Int, outputEdgeId: Int): CommandGraph = {
    // isCommandGraphNode = false ではこのメソッドは呼び出されない
    throw new AssertionError();
  }

  def toTreeString: List[String] = {
    "name: %s".format("cut") ::
    "cols: %s".format(cols.mkString(",")) ::
    Nil;
  }

}

