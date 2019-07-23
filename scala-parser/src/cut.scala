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
    (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]) = {
    if (opt == "--cols") {
      if (cols.isEmpty) {
        tail match {
          case arg2 :: tail2 =>
            (Right(status.copy(lastCommand = this.copy(cols = Some(arg2.split(",").toList)))),
              None, Some((tail2, argIdx + 2)));
          case Nil =>
            (Left(ParserErrorMessage(argIdx, "cols expected")),
              Some(completionCols), Some((tail, argIdx + 1)));
        }
      } else {
        (Left(ParserErrorMessage(argIdx, "duplicated option")), None, None);
      }
    } else {
      (Left(ParserErrorMessage(argIdx, "unknown option")), None, None);
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
        Right(Right(SomeCommandOptions(prevCommand, CutCommandNode(cols))));
    }
  }

}

case class CutCommandNode (
  cols: List[String],
) extends CommandNode {

  def toTreeString: List[String] = {
    "name: %s".format("cut") ::
    "cols: %s".format(cols.mkString(",")) ::
    Nil;
  }

}

