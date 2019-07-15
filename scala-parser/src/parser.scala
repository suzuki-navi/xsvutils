// mulang-bin-sources: scala

object OptionParser {

  object ForCommand {
    type CommandSeqParserStatus = OptionParser.CommandSeqParserStatus;
    type CommandParser = OptionParser.CommandParser;
    type CommandParserStatus = OptionParser.CommandParserStatus;
    type CommandOptions = OptionParser.CommandOptions;
    type ParserErrorMessage = OptionParser.ParserErrorMessage;
    val  ParserErrorMessage = OptionParser.ParserErrorMessage;
    type HelpDocument = OptionParser.HelpDocument;
  }

  def parse(args: List[String], argIdx: Int): Either[ParserMessage, CommandSeqOptions] = {
    parseCommandSeq(args, argIdx) match {
      case Left(e) =>
        Left(e);
      case Right((opt, args, argIdx)) =>
        args match {
          case Nil =>
            Right(opt);
          case "]" :: tail =>
            throw new AssertionError("TODO");
          case _ =>
            throw new AssertionError();
        }
    }
  }

  // コマンド列をパースする
  // パラメータ列の最後に達するか `--help` または `//help` または `]` に達するまでパースする
  // --help に達したら Left[HelpDocument] を返す
  def parseCommandSeq(args: List[String], argIdx: Int): Either[ParserMessage, (CommandSeqOptions, List[String], Int)] = {
    @scala.annotation.tailrec
    def sub(status: CommandSeqParserStatus, args: List[String], argIdx: Int): Either[ParserMessage, (CommandSeqOptions, List[String], Int)] = {
      args match {
        case Nil =>
          status.finish match {
            case Left(e) =>
              Left(e);
            case Right(opt) =>
              Right((opt, Nil, argIdx));
          }
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

    // 1つの引数またはオプションをパースする
    def parse(arg: String, tail: List[String], argIdx: Int): Either[ParserMessage, (CommandSeqParserStatus, List[String], Int)] = {
      parseCommandSeqParserStatus(this, arg, tail, argIdx);
    }

    def help: HelpDocument = lastCommand.help;

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

  sealed trait ParserMessage;
  case class ParserErrorMessage(argIdx: Int, message: String) extends ParserMessage;

  trait HelpDocument extends ParserMessage {
    def isFilePath: Boolean;
    def options: List[String];
    def commandsEnable: Boolean;
    def document: String;
  }

  def inputFileExists(file: String): Boolean = {
    val f = new java.io.File(file);
    f.exists && f.isFile;
  }

  def isOption(arg: String): Boolean = {
    arg.startsWith("-") || arg.startsWith("//");
    // `//` は `//help` を想定したもの
  }

  def isHelp(arg: String): Boolean = {
    arg == "//help" || arg == "--help";
  }

  trait CommandParser {
    def initStatus(lastCommand: CommandOptions, argIdx: Int): CommandParserStatus;
  }

  trait CommandParserStatus {

    def parseOption(status: CommandSeqParserStatus, opt: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)];

    def parseArgument(status: CommandSeqParserStatus, arg: String, tail: List[String], argIdx: Int): Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)];

    def help: HelpDocument;

    def finish: Either[ParserErrorMessage, CommandOptions];

  }

  trait CommandOptions {
  }

  val commands = Map (
    "cut" -> CutCommand.CutCommandParser,
  );

  object CommandParserStatus {

    def exists(name: String): Boolean = commands.contains(name);

    def command(name: String, lastCommand: CommandOptions, argIdx: Int): Option[CommandParserStatus] =
      commands.get(name).map(_.initStatus(lastCommand, argIdx));

  }

  private def parseCommandSeqParserStatus(status: CommandSeqParserStatus, arg: String, tail: List[String], argIdx: Int): Either[ParserMessage, (CommandSeqParserStatus, List[String], Int)] = {
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
                } else if (isHelp(file)) {
                  Left(Document.helpInputFile);
                } else {
                  Left(ParserErrorMessage(argIdx + 1, "file not found"));
                }
              case Nil =>
                Left(ParserErrorMessage(argIdx, "file path expected"));
            }
        }
      } else if (isHelp(arg)) {
        Left(status.help);
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

    def help: HelpDocument = Document.helpGlobalOptions;

    def finish: Either[ParserErrorMessage, CommandOptions] = Right(NoCommandOptions);

  }

  case object NoCommandOptions extends CommandOptions;

}
