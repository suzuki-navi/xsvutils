// mulang-bin-sources: scala

object CutCommand {

  import OptionParser.ForCommand._;

  case class CutCommandParserStatus (
    prevCommand: CommandOptions,
    argIdx: Int,
    cols: Option[List[String]],
  ) extends CommandParserStatus {

    def parseOption(status: CommandSeqParserStatus, opt: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
      Left(ParserErrorMessage(argIdx, "unknown option"));
    }

    def parseArgument(status: CommandSeqParserStatus, arg: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
      if (cols.isEmpty) {
        Right((status.copy(lastCommand = this.copy(cols = Some(arg.split(",").toList))), tail, argIdx + 1));
      } else {
        Left(ParserErrorMessage(argIdx, "unknown argument"));
      }
    }

    def finish: Either[ParserErrorMessage, CommandOptions] = {
      cols match {
        case None =>
          Left(ParserErrorMessage(argIdx, "expected --cols option"));
        case Some(cols) =>
          Right(CutCommandOptions(prevCommand, cols));
      }
    }

  }

  case class CutCommandOptions (
    prevCommand: CommandOptions,
    cols: List[String],
  ) extends CommandOptions {
  }

}

