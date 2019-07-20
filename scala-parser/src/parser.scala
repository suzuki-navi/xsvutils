// mulang-bin-sources: scala

object OptionParser {

  object ForCommand {
    type OptionParserContext = OptionParser.OptionParserContext;
    type CommandSeqParserStatus = OptionParser.CommandSeqParserStatus;
    type CommandSeqParserStatusImpl = OptionParser.CommandSeqParserStatusImpl;
    type CommandSeqOptions = OptionParser.CommandSeqOptions;
    type CompletionCommandSeqParserStatus = OptionParser.CompletionCommandSeqParserStatus;
    val  CompletionCommandSeqParserStatus = OptionParser.CompletionCommandSeqParserStatus;
    type CommandParser = OptionParser.CommandParser;
    type CommandParserStatus = OptionParser.CommandParserStatus;
    type CommandOptions = OptionParser.CommandOptions;
    type SomeCommandOptions = OptionParser.SomeCommandOptions;
    val  SomeCommandOptions = OptionParser.SomeCommandOptions;
    type CommandNode = CommandSeq.CommandNode;
    type ParserErrorMessage = OptionParser.ParserErrorMessage;
    val  ParserErrorMessage = OptionParser.ParserErrorMessage;
    type HelpDocument = OptionParser.HelpDocument;
    type Completion = OptionParser.Completion;
  }

  import CommandSeq.InputFormat;
  import CommandSeq.TsvInputFormat;
  import CommandSeq.CsvInputFormat;

  trait OptionParserContext {
    def inputFileExists(file: String): Boolean;
  }

  // OptionParser でのエントリーポイントとなるメソッド
  // 返り値の Boolean は --help かどうか
  def parseTotal(args: List[String], argIdx: Int, ctxt: OptionParserContext):
    Either[ParserErrorMessage, (CommandSeqParserStatus, Boolean)] = {
    parseCommandSeq(args, argIdx, ctxt) match {
      case (Left(e), _, _) =>
        Left(e);
      case (Right(status), args, argIdx) =>
        args match {
          case Nil =>
            Right((status, false));
          case "--help" :: tail =>
            Right((status, true));
          case "]" :: tail =>
            throw new AssertionError("TODO");
          case _ =>
            throw new AssertionError();
        }
    }
  }

  // コマンド列をパースする
  // パラメータ列の最後に達するか `--help` または `]` に達するまでパースする
  def parseCommandSeq(args: List[String], argIdx: Int, ctxt: OptionParserContext):
    (Either[ParserErrorMessage, CommandSeqParserStatus], List[String], Int) = {
    @scala.annotation.tailrec
    def sub(status: CommandSeqParserStatus, args: List[String], argIdx: Int):
      (Either[ParserErrorMessage, CommandSeqParserStatus], List[String], Int) = {
      args match {
        case Nil =>
          (Right(status), args, argIdx);
        case "--help" :: tail =>
          (Right(status), args, argIdx);
        case arg :: tail =>
          status.parse(arg, tail, argIdx, ctxt) match {
            case Left(e) =>
              (Left(e), args, argIdx);
            case Right((s, tail, idx)) =>
              sub(s, tail, idx);
          }
      }
    }
    sub(CommandSeqParserStatus.init, args, argIdx);
  }

  object CommandSeqParserStatus {
    def init: CommandSeqParserStatus = CommandSeqParserStatusImpl(
      None, None, None, 0, 0, NoneCommandParserStatus);
  }

  sealed trait CommandSeqParserStatus {

    // 1つの引数またはオプションをパースする
    def parse(arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)];

    def help: HelpDocument;

    def completion: Completion;

    // join, paste, union のパラメータとしてのサブコマンド
    //   input:  標準入力または外部ファイルまたはjoinなどの入力をtee
    //     inputType = 1;
    //   output: joinなどへの入力
    //     outputType = 2;
    // diff のパラメータとしてのサブコマンド
    //   input:  標準入力または外部ファイルまたはdiffの入力をtee
    //     inputType = 1;
    //   output: diffへの入力
    //     outputType = 2;
    // tee のパラメータとしてのサブコマンド
    //   input:  teeの入力をtee
    //     inputType = 2;
    //   output: 標準出力または外部ファイル
    //     outputType = 0;
    def finish(commandName: String, inputType: Int, outputType: Int):
      Either[ParserErrorMessage, CommandSeqOptions];

    def finish: Either[ParserErrorMessage, CommandSeqOptions] = finish("", 0, 0);

  }

  case class CommandSeqParserStatusImpl ( // TODO 名前がイケてない
    inputFormat: Option[InputFormat],
    inputFile: Option[String], // 空文字列は標準入力の意味
    outputFile: Option[String],
    inputFileArgIdx: Int,
    outputFileArgIdx: Int,
    lastCommand: CommandParserStatus,
  ) extends CommandSeqParserStatus {

    // 1つの引数またはオプションをパースする
    def parse(arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
      parseCommandSeqParserStatus(this, arg, tail, argIdx, ctxt);
    }

    def help: HelpDocument = lastCommand.help;

    def completion: Completion = lastCommand.completion;

    def finish(commandName: String, inputType: Int, outputType: Int):
      Either[ParserErrorMessage, CommandSeqOptions] = {
      try {
        lastCommand.finish match {
          case Left(err) =>
            Left(err);
          case Right(cmd) =>
            val inputFile2: Option[String] = if (inputType == 1) {
              inputFile match {
                case Some(f) => Some(f);
                case None    => None;
              }
            } else if (inputType == 2) {
              inputFile match {
                case Some(f) => throw new ParserException(inputFileArgIdx,
                  "input not allowed in a parameter of command `%s`".format(commandName));
                case None    => Some("");
              }
            } else {
              inputFile match {
                case Some(f) => Some(f);
                case None    => Some("");
              }
            }
            val outputFile2: Option[String] = if (outputType == 2) {
              outputFile match {
                case Some(f) => throw new ParserException(outputFileArgIdx,
                  "output not allowed in a parameter of command `%s`".format(commandName));
                case None    => None;
              }
            } else {
              outputFile match {
                case Some(f) => Some(f);
                case None    => Some("");
              }
            }
            Right(CommandSeqOptions(
              inputFormat,
              inputFile2,
              outputFile2,
              cmd));
        }
      } catch {
        case e: ParserException => Left(e.toMessage);
      }
    }

  }

  case class CommandSeqOptions (
    inputFormat: Option[InputFormat],
    inputFile: Option[String], // 空文字列は標準入力の意味、Noneはjoinなどの入力を引き継ぐ意味
    outputFile: Option[String], // 空文字列は標準出力の意味、Noneはjoinなどへの出力の意味
    lastCommand: CommandOptions,
  ) {

    def toCommandSeq: CommandSeq = {
      @scala.annotation.tailrec
      def sub(cmd: CommandOptions, seq: List[CommandSeq.CommandNode]): CommandSeq = {
        cmd match {
          case NoneCommandOptions =>
            CommandSeq(
              inputFormat,
              inputFile,
              outputFile,
              seq.toVector);
          case cmd: SomeCommandOptions =>
            sub(cmd.prevCommand, cmd.command :: seq);
        }
      }
      sub(lastCommand, Nil);
    }

  }

  case class ParserErrorMessage(argIdx: Int, message: String);

  class ParserException(argIdx: Int, message: String) extends Exception {
    def toMessage = ParserErrorMessage(argIdx, message);
  }

  trait HelpDocument {
    def document: List[String];
  }

  trait Completion {
    def isFilePath: Boolean;
    def options: List[String];
    def commandsEnable: Boolean;
  }

  // -i などパラメータをとるオプションで終わっていた場合にパースの結果として返すオブジェクト
  case class CompletionCommandSeqParserStatus(_completion: Completion, _err: ParserErrorMessage)
    extends CommandSeqParserStatus {

    def parse(arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
      throw new AssertionError(); // さらに引数を受け取ろうとすることはないはず
    }

    def help: HelpDocument = {
      throw new AssertionError();
    }

    def completion: Completion = _completion;

    def finish(commandName: String, inputType: Int, outputType: Int):
      Either[ParserErrorMessage, CommandSeqOptions] =
      Left(_err);

  }

  def isOption(arg: String): Boolean = {
    arg.startsWith("-");
  }

  trait CommandParser {
    def initStatus(lastCommand: CommandOptions, argIdx: Int): CommandParserStatus;
  }

  trait CommandParserStatus {

    def parseOption(status: CommandSeqParserStatusImpl,
      opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)];

    def parseArgument(status: CommandSeqParserStatusImpl,
      arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)];

    def help: HelpDocument;

    def completion: Completion;

    def finish: Either[ParserErrorMessage, CommandOptions];

  }

  sealed trait CommandOptions;

  // NoneCommandOptions 以外のすべての CommandOptions は SomeCommandOptions
  case class SomeCommandOptions (
    prevCommand: CommandOptions,
    command: CommandSeq.CommandNode) extends CommandOptions;

  val commands = Map (
    "cut" -> CutCommandParser,
    "diff" -> DiffCommandParser,
  );

  def commandExists(name: String): Boolean = commands.contains(name);

  def commandParserStatus(name: String, lastCommand: CommandOptions, argIdx: Int): Option[CommandParserStatus] =
    commands.get(name).map(_.initStatus(lastCommand, argIdx));

  private def parseCommandSeqParserStatus(status: CommandSeqParserStatusImpl,
    arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Either[ParserErrorMessage, (CommandSeqParserStatus, List[String], Int)] = {
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
      } else if (arg == "-") {
        // 入力は標準入力
        if (status.inputFile.isEmpty) {
          Right((status.copy(inputFile = Some(""), inputFileArgIdx = argIdx), tail, argIdx + 1));
        } else {
          Left(ParserErrorMessage(argIdx, "duplicated option"));
        }
      } else if (arg == "-i") {
        if (status.inputFile.isEmpty) {
          tail match {
            case arg2 :: tail2 =>
              if (ctxt.inputFileExists(arg2)) {
                Right((status.copy(inputFile = Some(arg2), inputFileArgIdx = argIdx), tail2, argIdx + 2));
              } else {
                Left(ParserErrorMessage(argIdx + 1, "file not found"));
              }
            case Nil =>
              Right((CompletionCommandSeqParserStatus(Document.completionInputFile,
                ParserErrorMessage(argIdx, "file path expected")), tail, argIdx + 1));
          }
        } else {
          Left(ParserErrorMessage(argIdx, "duplicated option"));
        }
      } else if (arg == "-o") {
        if (status.outputFile.isEmpty) {
          tail match {
            case arg2 :: tail2 =>
              Right((status.copy(outputFile = Some(arg2), outputFileArgIdx = argIdx), tail2, argIdx + 2));
            case Nil =>
              Right((CompletionCommandSeqParserStatus(Document.completionOutputFile,
                ParserErrorMessage(argIdx, "file path expected")), tail, argIdx + 1));
          }
        } else {
          Left(ParserErrorMessage(argIdx, "duplicated option"));
        }
      } else {
        status.lastCommand.parseOption(status, arg, tail, argIdx, ctxt);
      }
    } else { // - 以外で始まる引数
      if (commandExists(arg)) {
        // コマンド名と同じ名前のファイルが偶然存在していたとしてもコマンド名を優先する。
        // 実行環境による解釈の揺れを少しでも減らすため。
        status.lastCommand.finish match {
          case Left(err) =>
            Left(err);
          case Right(cmd) =>
            Right((status.copy(lastCommand = commandParserStatus(arg, cmd, argIdx).get), tail, argIdx + 1));
        }
      } else if (ctxt.inputFileExists(arg)) {
        if (status.inputFile.isEmpty) {
          Right((status.copy(inputFile = Some(arg), inputFileArgIdx = argIdx), tail, argIdx + 1));
        } else {
          Left(ParserErrorMessage(argIdx, "duplicated parameter"));
        }
      } else {
        status.lastCommand.parseArgument(status, arg, tail, argIdx, ctxt);
      }
    }
  }

  case object NoneCommandParserStatus extends CommandParserStatus {

    def parseOption(status: CommandSeqParserStatusImpl,
      opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      Either[ParserErrorMessage, (CommandSeqParserStatusImpl, List[String], Int)] = {
      Left(ParserErrorMessage(argIdx, "unknown option"));
    }

    def parseArgument(status: CommandSeqParserStatusImpl,
      arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      Either[ParserErrorMessage, (CommandSeqParserStatusImpl, List[String], Int)] = {
      Left(ParserErrorMessage(argIdx, "unknown argument"));
    }

    def help: HelpDocument = Document.helpGlobalOptions;

    def completion: Completion = Document.completionGlobalOptions;

    def finish: Either[ParserErrorMessage, CommandOptions] = Right(NoneCommandOptions);

  }

  case object NoneCommandOptions extends CommandOptions;

}
