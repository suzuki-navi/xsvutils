// mulang-bin-sources: scala

object OptionParser {

  object ForCommand {
    type OptionParserContext = OptionParser.OptionParserContext;
    type CommandSeqParserStatus = OptionParser.CommandSeqParserStatus;
    val  CommandSeqParserStatus = OptionParser.CommandSeqParserStatus;
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
    type CommandSeqReceiver = OptionParser.CommandSeqReceiver;
  }

  import CommandSeq.InputFormat;
  import CommandSeq.TsvInputFormat;
  import CommandSeq.CsvInputFormat;

  trait OptionParserContext {
    def inputFileExists(file: String): Boolean;
  }

  // OptionParser でのエントリーポイントとなるメソッド
  // 
  // 通常実行時に返すべきオブジェクト
  //   ParserErrorMessage
  //   CommandSeq
  //   HelpDocument
  // 
  // 補完実行時に返すべきオブジェクト
  //   ParserErrorMessage
  //   Completion
  //   HelpDocument
  // 
  // 返すパターン
  //   ParserErrorMessage
  //   ParserErrorMessage, Completion
  //   CommandSeq, Completion
  //   HelpDocument
  // 
  // ParserResult は以下のいずれか
  //   ParserErrorMessage
  //   HelpDocument
  def parse(args: List[String], ctxt: OptionParserContext): (Either[ParserResult, CommandSeq], Option[Completion]) = {
    @scala.annotation.tailrec
    def sub(status: CommandSeqParserStatus, args: List[String], argIdx: Int):
      (Either[ParserResult, CommandSeq], Option[Completion]) = {
      args match {
        case Nil =>
          status.endReceiver match {
            case Some(endReceiver) =>
              endReceiver.receive(status) match {
                case (Left(e), completionOpt) =>
                  (Left(e), completionOpt);
                case (Right(status2), completionOpt) =>
                  status2.finish match {
                    case Left(e) =>
                      // 構文エラー
                      (Left(e), completionOpt);
                    case Right(cmds) =>
                      (Right(cmds), completionOpt);
                  }
              }
            case None =>
              status.finish match {
                case Left(e) =>
                  // 構文エラー
                  (Left(e), Some(status.completion));
                case Right(cmds) =>
                  (Right(cmds), Some(status.completion));
              }
          }
        case "--help" :: tail =>
          (Left(status.help), None);
        case "]" :: tail =>
          status.endReceiver match {
            case Some(endReceiver) =>
              endReceiver.receive(status) match {
                case (Left(e), completionOpt) =>
                  (Left(e), completionOpt);
                case (Right(s), _) =>
                  sub(s, args, argIdx);
              }
            case None =>
              status.closeReceiver match {
                case Some(closeReceiver) =>
                  closeReceiver.receive(status) match {
                    case (Left(e), completionOpt) =>
                      (Left(e), completionOpt);
                    case (Right(s), _) =>
                      sub(s, tail, argIdx + 1);
                  }
                case None =>
                  (Left(ParserErrorMessage(argIdx, "unexpected `]`")), None);
              }
          }
        case arg :: tail =>
          status.parse(arg, tail, argIdx, ctxt) match {
            case (Left(e), cmpletionOpt, _) =>
              // 構文エラー
              (Left(e), cmpletionOpt);
            case (Right(s), _, Some((tail, idx))) =>
              sub(s, tail, idx);
            case (Right(s), _, _) =>
              throw new AssertionError();
          }
      }
    }
    sub(CommandSeqParserStatus.init, args, 0);
  }

  sealed trait ParserResult;

  object CommandSeqParserStatus {
    def init: CommandSeqParserStatus = CommandSeqParserStatus(
      endReceiver = None,
      closeReceiver = None,
      inputFormat = None,
      inputFile = None,
      outputFile = None,
      inputFileArgIdx = 0,
      outputFileArgIdx = 0,
      lastCommand = NoneCommandParserStatus);
  }

  case class CommandSeqParserStatus (
    endReceiver: Option[CommandSeqReceiver],
    closeReceiver: Option[CommandSeqReceiver],
    inputFormat: Option[InputFormat],
    inputFile: Option[String], // 空文字列は標準入力の意味
    outputFile: Option[String],
    inputFileArgIdx: Int,
    outputFileArgIdx: Int,
    lastCommand: CommandParserStatus,
  ) {

    // 1つの引数またはオプションをパースする
    def parse(arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]) = {
      parseCommandSeqParserStatus(this, arg, tail, argIdx, ctxt);
    }

    def help: HelpDocument = lastCommand.help;

    def completion: Completion = lastCommand.completion;

    def finish: Either[ParserErrorMessage, CommandSeq] = finish("", 0, 0);

    // inputType, outputType について
    //   join, paste, union のパラメータとしてのサブコマンド
    //     input:  標準入力または外部ファイルまたはjoinなどの入力をtee
    //       inputType = 1;
    //     output: joinなどへの入力
    //       outputType = 2;
    //   diff のパラメータとしてのサブコマンド
    //     input:  標準入力または外部ファイルまたはdiffの入力をtee
    //       inputType = 1;
    //     output: diffへの入力
    //       outputType = 2;
    //   tee のパラメータとしてのサブコマンド
    //     input:  teeの入力をtee
    //       inputType = 2;
    //     output: 標準出力または外部ファイル
    //       outputType = 0;
    def finish(commandName: String, inputType: Int, outputType: Int):
      Either[ParserErrorMessage, CommandSeq] = {
      try {
        lastCommand.finish(this) match {
          case Left(receiver) =>
            receiver.receive(CommandSeqParserStatus.init) match {
              case (Left(e), _) =>
                Left(e);
              case (Right(s), _) =>
                s.finish(commandName, inputType, outputType);
            }
          case Right(Left(err)) =>
            Left(err);
          case Right(Right(cmd)) =>
            val inputFile2: Option[String] = if (inputType == 1) {
              inputFile match {
                case Some(f) => Some(f);
                case None    => None;
              }
            } else if (inputType == 2) {
              inputFile match {
                case Some(f) => throw new ParserException(inputFileArgIdx,
                  "input not allowed in a parameter of command `%s`".format(commandName));
                case None    => None;
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
            Right(CommandSeq(
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

  case class ParserErrorMessage(argIdx: Int, message: String) extends ParserResult;

  class ParserException(argIdx: Int, message: String) extends Exception {
    def toMessage = ParserErrorMessage(argIdx, message);
  }

  trait HelpDocument extends ParserResult {
    def document: List[String];
  }

  trait Completion {
    def isFilePath: Boolean;
    def options: List[String];
    def commandsEnable: Boolean;
  }

  def isOption(arg: String): Boolean = {
    arg.startsWith("-") || arg == "[";
  }

  trait CommandParser {
    def initStatus(lastCommand: CommandOptions, argIdx: Int): CommandParserStatus;
  }

  trait CommandParserStatus {

    def parseOption(status: CommandSeqParserStatus,
      opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]);

    def parseArgument(status: CommandSeqParserStatus,
      arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]);

    def help: HelpDocument;

    def completion: Completion;

    def finish(status: CommandSeqParserStatus):
      Either[CommandSeqReceiver, Either[ParserErrorMessage, CommandOptions]];

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

  // 返すパターン
  //   ParserErrorMessage
  //   ParserErrorMessage, Completion
  //   CommandSeq, tail
  private def parseCommandSeqParserStatus(status: CommandSeqParserStatus,
    arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]) = {
    if (isOption(arg)) {
      if (arg == "--tsv") {
        status.inputFormat match {
          case Some(_) =>
            (Left(ParserErrorMessage(argIdx, "duplicated option")), None, None);
          case None =>
            (Right(status.copy(inputFormat = Some(TsvInputFormat))), None, Some((tail, argIdx + 1)));
        }
      } else if (arg == "--csv") {
        status.inputFormat match {
          case Some(_) =>
            (Left(ParserErrorMessage(argIdx, "duplicated option")), None, None);
          case None =>
            (Right(status.copy(inputFormat = Some(CsvInputFormat))), None, Some((tail, argIdx + 1)));
        }
      } else if (arg == "-") {
        // 入力は標準入力
        if (status.inputFile.isEmpty) {
          (Right(status.copy(inputFile = Some(""), inputFileArgIdx = argIdx)), None, Some((tail, argIdx + 1)));
        } else {
          (Left(ParserErrorMessage(argIdx, "duplicated option")), None, None);
        }
      } else if (arg == "-i") {
        if (status.inputFile.isEmpty) {
          tail match {
            case arg2 :: tail2 =>
              if (ctxt.inputFileExists(arg2)) {
                (Right(status.copy(inputFile = Some(arg2), inputFileArgIdx = argIdx)), None, Some((tail2, argIdx + 2)));
              } else {
                (Left(ParserErrorMessage(argIdx + 1, "file not found")), None, None);
              }
            case Nil =>
              (Left(ParserErrorMessage(argIdx, "file path expected")), Some(Document.completionInputFile), None);
          }
        } else {
          (Left(ParserErrorMessage(argIdx, "duplicated option")), None, None);
        }
      } else if (arg == "-o") {
        if (status.outputFile.isEmpty) {
          tail match {
            case arg2 :: tail2 =>
              (Right(status.copy(outputFile = Some(arg2), outputFileArgIdx = argIdx)),
                None, Some((tail2, argIdx + 2)));
            case Nil =>
              (Left(ParserErrorMessage(argIdx, "file path expected")), Some(Document.completionOutputFile), None);
          }
        } else {
          (Left(ParserErrorMessage(argIdx, "duplicated option")), None, None);
        }
      } else {
        status.lastCommand.parseOption(status, arg, tail, argIdx, ctxt);
      }
    } else { // - 以外で始まる引数
      if (commandExists(arg)) {
        // コマンド名と同じ名前のファイルが偶然存在していたとしてもコマンド名を優先する。
        // 実行環境による解釈の揺れを少しでも減らすため。
        status.lastCommand.finish(status) match {
          case Left(receiver) =>
            parseCommandSeqParserStatus(CommandSeqParserStatus.init.copy(endReceiver = Some(receiver)),
              arg, tail, argIdx, ctxt);
          case Right(Left(err)) =>
            (Left(err), None, None);
          case Right(Right(cmd)) =>
            (Right(status.copy(lastCommand = commandParserStatus(arg, cmd, argIdx).get)),
              None, Some((tail, argIdx + 1)));
        }
      } else if (ctxt.inputFileExists(arg) && status.inputFile.isEmpty) {
        (Right(status.copy(inputFile = Some(arg), inputFileArgIdx = argIdx)), None, Some((tail, argIdx + 1)));
      } else if (ctxt.inputFileExists(arg) && status.lastCommand == NoneCommandParserStatus) {
        (Left(ParserErrorMessage(argIdx, "duplicated parameter")), None, None);
      } else if (!ctxt.inputFileExists(arg) && status.lastCommand == NoneCommandParserStatus) {
        (Left(ParserErrorMessage(argIdx, "file not found")), None, None);
      } else {
        status.lastCommand.parseArgument(status, arg, tail, argIdx, ctxt);
      }
    }
  }

  trait CommandSeqReceiver {

    // メソッド実装時の注意
    // endReceiver として設定する CommandSeqReceiver では
    // 返り値の CommandSeqParserStatus の endReceiver は None である必要がある。
    def receive(status: CommandSeqParserStatus):
      (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion]);

  }

  case object NoneCommandParserStatus extends CommandParserStatus {

    def parseOption(status: CommandSeqParserStatus,
      opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]) = {
      (Left(ParserErrorMessage(argIdx, "unknown option")), None, None);
    }

    def parseArgument(status: CommandSeqParserStatus,
      arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
      (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]) = {
      (Left(ParserErrorMessage(argIdx, "unknown argument")), None, None);
    }

    def help: HelpDocument = Document.helpGlobalOptions;

    def completion: Completion = Document.completionGlobalOptions;

    def finish(status: CommandSeqParserStatus):
      Either[CommandSeqReceiver, Either[ParserErrorMessage, CommandOptions]] =
      Right(Right(NoneCommandOptions));

  }

  case object NoneCommandOptions extends CommandOptions;

}
