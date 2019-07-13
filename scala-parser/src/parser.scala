// mulang-bin-sources: scala

object Main {
  def main(args: Array[String]): Unit = {
    OptionParser.parse(args.toList, 0) match {
      case Right(status) =>
        println(status);
        //printDocuments(documents);
      case Left(OptionParser.ParserErrorMessage(argIdx, message)) =>
        println("error: (%d) \"%s\": %s".format(argIdx, args(argIdx), message));
        //printDocuments(documents);
    }
  }
}

object OptionParser {

  def parse(args: List[String], argIdx: Int): Either[ParserErrorMessage, CommandSeqOptions] = {
    val initArgs = args;
    @scala.annotation.tailrec
    def sub(status: CommandSeqParserStatus, args: List[String], argIdx: Int): Either[ParserErrorMessage, CommandSeqOptions] = {
      args match {
        case Nil =>
          status.finish;
        case arg :: tail =>
          status.parse(arg, tail, argIdx) match {
            case Left(e) =>
              Left(e);
            case Right((s, tail, idx)) =>
              sub(s, tail, idx);
          }
      }
    }
    sub(CommandSeqParserStatus.init, args, argIdx);
  }

  object CommandSeqParserStatus {
    def init = CommandSeqParserStatus(None, None, NoCommandParserStatus);
  }

  case class CommandSeqParserStatus (
    inputFormat: Option[InputFormat],
    inputFile: Option[String],
    lastCommand: CommandParserStatus,
  ) {

    def parse(arg: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
      parseCommandSeqParserStatus(this, arg, tail, argIdx);
    }

    def finish: Either[ParserErrorMessage, CommandSeqOptions] = {
      lastCommand.finish match {
        case Left(err) =>
          Left(err);
        case Right(cmd) =>
          Right(CommandSeqOptions(inputFormat, inputFile, cmd));
      }
    }

  }

  case class CommandSeqOptions (
    inputFormat: Option[InputFormat],
    inputFile: Option[String],
    lastCommand: CommandOptions,
  ) {
  }

  sealed trait InputFormat;
  case object TsvInputFormat extends InputFormat;
  case object CsvInputFormat extends InputFormat;

  case class ParserErrorMessage(argIdx: Int, message: String);

  def inputFileExists(file: String): Boolean = {
    true; // TODO
  }

  def isOption(arg: String): Boolean = {
    arg.startsWith("-");
  }

//  trait ArgDocument;
//
//  case object GlobalOptionsDocument extends ArgDocument;

  trait CommandParserStatus {

    def parseOption(status: CommandSeqParserStatus, opt: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)];

    def parseArgument(status: CommandSeqParserStatus, arg: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)];

    def finish: Either[ParserErrorMessage, CommandOptions];

  }
  trait CommandOptions {
  }

  object CommandParserStatus {
    def exists(name: String): Boolean = {
      name match {
        case "cut" => true;
        case _ => false;
      }
    }
    def command(name: String, lastCommand: CommandOptions, argIdx: Int): Option[CommandParserStatus] = {
      name match {
        case "cut" => Some(CutCommandParserStatus(lastCommand, argIdx, None));
        case _ => None;
      }
    }
  }

  private def parseCommandSeqParserStatus(status: CommandSeqParserStatus, arg: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
    if (isOption(arg)) {
      if (arg == "--tsv") {
        status.inputFormat match {
          case Some(_) => Left(ParserErrorMessage(argIdx, "duplicated option"));
          case None => Right((status.copy(inputFormat = Some(TsvInputFormat)), tail, argIdx + 1));
        }
      } else if (arg == "--csv") {
        status.inputFormat match {
          case Some(_) => Left(ParserErrorMessage(argIdx, "duplicated option"));
          case None => Right((status.copy(inputFormat = Some(CsvInputFormat)), tail, argIdx + 1));
        }
      } else if (arg == "-i") {
        status.inputFile match {
          case Some(_) => Left(ParserErrorMessage(argIdx, "duplicated option"));
          case None =>
            tail match {
              case file :: tail2 =>
                if (inputFileExists(file)) {
                  Right((status.copy(inputFile = Some(file)), tail2, argIdx + 2));
                } else {
                  Left(ParserErrorMessage(argIdx + 1, "file not found"));
                }
              case Nil =>
                Left(ParserErrorMessage(argIdx, "file path expected"));
            }
        }
      } else {
        status.lastCommand.parseOption(status, arg, tail, argIdx);
      }
    } else if (CommandParserStatus.exists(arg)) {
      status.lastCommand.finish match {
        case Left(err) =>
          Left(err);
        case Right(cmd) =>
          Right((status.copy(lastCommand = CommandParserStatus.command(arg, cmd, argIdx).get), tail, argIdx + 1));
      }
    } else {
      status.lastCommand.parseArgument(status, arg, tail, argIdx);
    }
  }

  case object NoCommandParserStatus extends CommandParserStatus {

    def parseOption(status: CommandSeqParserStatus, opt: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
      Left(ParserErrorMessage(argIdx, "unknown option"));
    }

    def parseArgument(status: CommandSeqParserStatus, arg: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
      Left(ParserErrorMessage(argIdx, "unknown argument"));
    }

    def finish: Either[ParserErrorMessage, CommandOptions] = Right(NoCommandOptions);

  }

  case object NoCommandOptions extends CommandOptions;

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
