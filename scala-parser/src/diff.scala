// mulang-bin-sources: scala

import OptionParser.ForCommand._;

object DiffCommandParser extends CommandParser {
  def initStatus(lastCommand: CommandOptions, argIdx: Int): CommandParserStatus =
    DiffCommandParserStatus(lastCommand, argIdx, None);
}

case class DiffCommandParserStatus (
  prevCommand: CommandOptions,
  argIdx: Int,
  otherFile: Option[Either[String, CommandSeqOptions]],
) extends CommandParserStatus {

  def parseOption(status: CommandSeqParserStatusImpl,
    opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
    if (opt == "--file") {
      if (otherFile.isEmpty) {
        tail match {
          case arg2 :: tail2 =>
            if (ctxt.inputFileExists(arg2)) {
              Right((status.copy(lastCommand = this.copy(otherFile = Some(Left(arg2)))), tail2, argIdx + 2));
            } else {
              Left(ParserErrorMessage(argIdx + 1, "file not found"));
            }
          case Nil =>
            Right((CompletionCommandSeqParserStatus(completionFile,
              ParserErrorMessage(argIdx, "file path expected")), tail, argIdx + 1));
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
//      if (otherFile.isEmpty) {
//        Right((status.copy(lastCommand = this.copy(cols = Some(arg.split(",").toList))), tail, argIdx + 1));
//      } else {
      Left(ParserErrorMessage(argIdx, "unknown argument"));
//      }
  }

  def help = new HelpDocument {
    def document: List[String] = List(
      "TODO",
    );
  }

  def completion = new Completion {
    def isFilePath: Boolean = true; // TODO
    def options: List[String] = Nil; // TODO
    def commandsEnable: Boolean = false; // TODO
  }

  private[this] def completionFile = new Completion {
    def isFilePath: Boolean = true;
    def options: List[String] = Nil;
    def commandsEnable: Boolean = false;
  }

  def finish: Either[ParserErrorMessage, CommandOptions] = {
    otherFile match {
      case None =>
        Left(ParserErrorMessage(argIdx, "expected --file option"));
      case Some(otherFile) =>
        Right(SomeCommandOptions(prevCommand, DiffCommandNode(otherFile)));
    }
  }

}

case class DiffCommandNode (
  otherFile: Either[String, CommandSeqOptions],
) extends CommandNode {
}

