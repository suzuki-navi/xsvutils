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

  def parseOption(status: CommandSeqParserStatusImpl,
    opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
    if (opt == "--cols") {
      if (cols.isEmpty) {
        tail match {
          case arg2 :: tail2 =>
            Right((status.copy(lastCommand = this.copy(cols = Some(arg2.split(",").toList))), tail2, argIdx + 2));
          case Nil =>
            Right((CompletionCommandSeqParserStatus(completionCols,
              ParserErrorMessage(argIdx, "cols expected")), tail, argIdx + 1));
        }
      } else {
        Left(ParserErrorMessage(argIdx, "duplicated option"));
      }
    } else {
      Left(ParserErrorMessage(argIdx, "unknown option"));
    }
  }

  def parseArgument(status: CommandSeqParserStatusImpl,
    arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
    if (cols.isEmpty) {
      Right((status.copy(lastCommand = this.copy(cols = Some(arg.split(",").toList))), tail, argIdx + 1));
    } else {
      Left(ParserErrorMessage(argIdx, "unknown argument"));
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

  def finish: Either[ParserErrorMessage, CommandOptions] = {
    cols match {
      case None =>
        Left(ParserErrorMessage(argIdx, "expected --cols option"));
      case Some(cols) =>
        Right(SomeCommandOptions(prevCommand, CutCommandNode(cols)));
    }
  }

}

case class CutCommandNode (
  cols: List[String],
) extends CommandNode {
}

