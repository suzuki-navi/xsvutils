// mulang-bin-sources: scala

object OptionParser {

  def parseCommands(args: List[String], ctxt: OptionParserContext):
    Either3[CommandSeqParserStatus, ParserErrorMessage, HelpDocument] = {
    parse(args, false, ctxt) match {
      case Option4A(status) => Option3A(status);
      case Option4B(err) =>    Option3B(err);
      case Option4C(help) =>   Option3C(help);
      case Option4D(cmpl) =>   throw new AssertionError(); // ここにはこないはず
    }
  }

  def parseCompletion(args: List[String], ctxt: OptionParserContext): Option[Completion] = {
    parse(args, true, ctxt) match {
      case Option4A(status) => None;
      case Option4B(err) =>    None;
      case Option4C(help) =>   None;
      case Option4D(cmpl) =>   Some(cmpl);
    }
  }

  private[this] def parse(args: List[String], isCompletion: Boolean, ctxt: OptionParserContext):
    Either4[CommandSeqParserStatus, ParserErrorMessage, HelpDocument, Completion] = {

    @scala.annotation.tailrec
    def sub(status: CommandSeqParserStatus, args: List[String], argIdx: Int):
      Either4[CommandSeqParserStatus, ParserErrorMessage, HelpDocument, Completion] = {
      args match {
        case Nil =>
          Option4A(status);
        case arg :: tail =>
          status.eat(arg, tail, argIdx, isCompletion, ctxt) match {
            case (Option5A(status), Some((args, argIdx))) =>
              sub(status, args, argIdx);
            case (Option5A(status), None) =>
              Option4A(status);
            case (Option5B(err), _) =>
              Option4B(err);
            case (Option5C(help), _) =>
              Option4C(help);
            case (Option5D(cmpl), _) =>
              Option4D(cmpl);
            case (Option5E(SeqEnd), _) =>
              Option4B(ParserErrorMessage(argIdx, "unexpected \"]\""));
          }
      }
    }

    val status = CommandSeqParserStatus.init(CommandSeqInputType.SomeInput, CommandSeqOutputType.SomeOutput);
    sub(status, args, 0);
  }

  def isOption(arg: String): Boolean = {
    arg.startsWith("-") || arg == "[" || arg == "]";
  }

  val commands: Map[String, CommandParser] = Map (
    "cut" -> CutCommandParser,
    "diff" -> DiffCommandParser,
  );

  def commandExists(name: String): Boolean = commands.contains(name);

  def command(name: String, argIdx: Int): CommandParserStatus = {
    commands(name).initStatus(argIdx);
  }

}

trait OptionParserContext {
  def inputFileExists(file: String): Boolean;
}

// パース処理以降でこのオブジェクトに対して実行できる内容
// - 実行
// - 入力途中とみなして
//   - ヘルプ表示
//     - 入力中のコマンド
//     - コマンド名の前であればグローバルオプション
//   - シェル補完
//     - 次のコマンド名
//     - 入力中のコマンドのオプション
//     - カラム名
//     - 入出力ファイル名
case class CommandSeqParserStatus (
  inputType: CommandSeqInputType,
  outputType: CommandSeqOutputType,
  inputFormat: Option[InputFormat],
  inputFile: Option[String], // 空文字列は標準入力の意味
  outputFile: Option[String],
  inputFileArgIdx: Int,
  outputFileArgIdx: Int,
  commands: Vector[CommandNode],
  lastCommand: CommandParserStatus,
  childOrTailStatus: Option[Either[CommandSeqParserStatus, CommandSeqParserStatus]],
) {

  def eat(arg: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    (Either5[CommandSeqParserStatus, ParserErrorMessage, HelpDocument, Completion, SeqEnd.type],
      Option[(List[String], Int)]) = {
    lastCommand.childParser match {
      case None =>
        eat3(arg, tail, argIdx, isCompletion, ctxt);
      case Some(childParser) =>
        this.copy(childOrTailStatus = Some(Left(childParser.initStatus))).
          eat(arg, tail, argIdx, isCompletion, ctxt);
    }
  }

  private[this] def eat3(arg: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    (Either5[CommandSeqParserStatus, ParserErrorMessage, HelpDocument, Completion, SeqEnd.type],
      Option[(List[String], Int)]) = {
    childOrTailStatus match {
      case None =>
        eat4(arg, tail, argIdx, isCompletion, ctxt);
      case Some(Left(childStatus)) =>
        childStatus.eat(arg, tail, argIdx, isCompletion, ctxt) match {
          case (Option5A(status), opt) =>
            (Option5A(this.copy(childOrTailStatus = Some(Left(status)))), opt);
          case (Option5B(err), opt) =>
            (Option5B(err), opt);
          case (Option5C(help), opt) =>
            (Option5C(help), opt);
          case (Option5D(cmpl), opt) =>
            //val cmpl2 = new Completion { // TODO "]" を補完
            //}
            (Option5D(cmpl), opt);
          case (Option5E(SeqEnd), opt) =>
            childStatus.finish match {
              case Left(err) =>
                (Option5B(err), None);
              case Right(cmds) =>
                val newLastCommand = lastCommand.childParser.get.endStatus(cmds);
                (Option5A(this.copy(lastCommand = newLastCommand, childOrTailStatus = None)),
                  Some((tail, argIdx + 1)));
            }
        }
      case Some(Right(tailStatus)) =>
        tailStatus.eat(arg, tail, argIdx, isCompletion, ctxt) match {
          case (Option5A(status), opt) =>
            (Option5A(this.copy(childOrTailStatus = Some(Right(status)))), opt);
          case (Option5B(err), opt) =>
            (Option5B(err), opt);
          case (Option5C(help), opt) =>
            (Option5C(help), opt);
          case (Option5D(cmpl), opt) =>
            (Option5D(cmpl), opt);
          case (Option5E(SeqEnd), opt) =>
            (Option5E(SeqEnd), opt);
        }
    }
  }

  private[this] def eat4(arg: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    (Either5[CommandSeqParserStatus, ParserErrorMessage, HelpDocument, Completion, SeqEnd.type],
      Option[(List[String], Int)]) = {
    if (tail.isEmpty && isCompletion) {
      if (lastCommand == NoneCommandParserStatus) {
        (Option5D(this.completion), None);
      } else {
        (Option5D(lastCommand.completion), None);
      }
    } else if (OptionParser.isOption(arg)) {
      if (arg == "]") {
        (Option5E(SeqEnd), Some(arg :: tail, argIdx));
      } else if (arg == "--help") {
        // --help はすべてのコマンドで共通のオプションであるため最初にチェック
        (Option5C(lastCommand.help), None);
      } else {
        // コマンドのオプションとして解釈を試みる
        lastCommand.eatOption(arg, tail, argIdx, isCompletion, ctxt) match {
          case None =>
            // コマンドのオプションとして解釈できなかったケース
            // グローバルのオプションとして解釈を試みる
            eatOption(arg, tail, argIdx, isCompletion, ctxt) match {
              case (Option3A(status), opt) =>
                (Option5A(status), opt);
              case (Option3B(err), opt) =>
                (Option5B(err), opt);
              case (Option3C(cmpl), opt) =>
                (Option5D(cmpl), opt);
            }
          case Some((Option3A(cmd), opt)) =>
            // コマンドのオプションとして解釈できたケース
            (Option5A(this.copy(lastCommand = cmd)), opt);
          case Some((Option3B(err), opt)) =>
            // コマンドのオプションとして解釈できたけどエラーのケース
            (Option5B(err), opt);
          case Some((Option3C(cmpl), opt)) =>
            (Option5D(cmpl), opt);
        }
      }
    } else { // - 以外で始まる引数
      if (OptionParser.commandExists(arg)) {
        // コマンド名と同じ名前のファイルが偶然存在していたとしてもコマンド名を優先する。
        // 実行環境による解釈の揺れを少しでも減らすため。
        val nextCommand = OptionParser.command(arg, argIdx);
        lastCommand.tailParser match {
          case None =>
            if (lastCommand == NoneCommandParserStatus) {
              (Option5A(this.copy(lastCommand = nextCommand)),
                Some((tail, argIdx + 1)));
            } else {
              lastCommand.finish match {
                case Left(err) =>
                  (Option5B(err), None);
                case Right(cmd) =>
                  (Option5A(this.copy(commands = commands :+ cmd, lastCommand = nextCommand)),
                    Some((tail, argIdx + 1)));
              }
            }
          case Some(tailParser) =>
            (Option5A(this.copy(childOrTailStatus = Some(Right(tailParser.initStatus)))),
              Some((arg :: tail, argIdx)));
        }
      } else if (ctxt.inputFileExists(arg) && inputFile.isEmpty) {
        (Option5A(this.copy(inputFile = Some(arg), inputFileArgIdx = argIdx)), Some((tail, argIdx + 1)));
      } else if (ctxt.inputFileExists(arg) && lastCommand == NoneCommandParserStatus) {
        (Option5B(ParserErrorMessage(argIdx, "duplicated parameter")), None);
      } else if (!ctxt.inputFileExists(arg) && lastCommand == NoneCommandParserStatus) {
        (Option5B(ParserErrorMessage(argIdx, "file not found")), None);
      } else {
        lastCommand.eatArgument(arg, tail, argIdx, isCompletion, ctxt) match {
          case Some((Option3A(cmd), opt)) =>
            (Option5A(this.copy(lastCommand = cmd)), opt);
          case Some((Option3B(err), opt)) =>
            (Option5B(err), opt);
          case Some((Option3C(cmpl), opt)) =>
            (Option5D(cmpl), opt);
          case None =>
            (Option5B(ParserErrorMessage(argIdx, "unknown argument")), None);
        }
      }
    }
  }

  private[this] def eatOption(opt: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    (Either3[CommandSeqParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)]) = {
    if (opt == "--tsv") {
      inputFormat match {
        case Some(_) =>
          (Option3B(ParserErrorMessage(argIdx, "duplicated option")), None);
        case None =>
          (Option3A(this.copy(inputFormat = Some(TsvInputFormat))),
            Some((tail, argIdx + 1)));
      }
    } else if (opt == "--csv") {
      inputFormat match {
        case Some(_) =>
          (Option3B(ParserErrorMessage(argIdx, "duplicated option")), None);
        case None =>
          (Option3A(this.copy(inputFormat = Some(CsvInputFormat))),
            Some((tail, argIdx + 1)));
      }
    } else if (opt == "-") {
      inputFile match {
        case Some(_) =>
          (Option3B(ParserErrorMessage(argIdx, "duplicated option")), None);
        case None =>
          if (!inputType.enableInputFileParam) {
            (Option3B(ParserErrorMessage(argIdx, "input not allowed here")), None);
          } else {
            (Option3A(this.copy(inputFile = Some(""), inputFileArgIdx = argIdx)),
              Some((tail, argIdx + 1)));
          }
      }
    } else if (opt == "-i") {
      inputFile match {
        case Some(_) =>
          (Option3B(ParserErrorMessage(argIdx, "duplicated option")), None);
        case None =>
          if (!inputType.enableInputFileParam) {
            (Option3B(ParserErrorMessage(argIdx, "input not allowed here")), None);
          } else {
            tail match {
              case Nil =>
                (Option3B(ParserErrorMessage(argIdx, "file path expected")), None);
              case arg :: Nil if isCompletion =>
                (Option3C(fileCompletion), None);
              case arg :: tail2 =>
                if (ctxt.inputFileExists(arg)) {
                  (Option3A(this.copy(inputFile = Some(arg), inputFileArgIdx = argIdx + 1)),
                    Some((tail2, argIdx + 2)));
                } else {
                  (Option3B(ParserErrorMessage(argIdx + 1, "file not found")), None);
                }
            }
          }
      }
    } else if (opt == "-o") {
      outputFile match {
        case Some(_) =>
          (Option3B(ParserErrorMessage(argIdx, "duplicated option")), None);
        case None =>
          if (!outputType.enableOutputFileParam) {
            (Option3B(ParserErrorMessage(argIdx, "output not allowed here")), None);
          } else {
            tail match {
              case Nil =>
                (Option3B(ParserErrorMessage(argIdx, "file path expected")), None);
              case arg :: Nil if isCompletion =>
                (Option3C(fileCompletion), None);
              case arg :: tail2 =>
                (Option3A(this.copy(outputFile = Some(arg), outputFileArgIdx = argIdx + 1)),
                  Some((tail2, argIdx + 2)));
            }
          }
      }
    } else {
      (Option3B(ParserErrorMessage(argIdx, "unknown option")), None);
    }
  }

  def finish: Either[ParserErrorMessage, CommandNodeSeq] = {
    throw new AssertionError("TODO");
  }

  private[this] def completion = new Completion {
    def isFilePath: Boolean = {
      if (inputFile.isEmpty) {
        true;
      } else {
        false;
      }
    }

    def parameters: List[String] = Nil;

    def options: List[String] = {
      (if (inputFile.isEmpty) {
        "-i" :: "-" :: Nil;
      } else {
        Nil;
      }) :::
      (if (outputFile.isEmpty) {
        "-o" :: Nil;
      } else {
        Nil;
      }) :::
      (if (inputFormat.isEmpty) {
        "--tsv" :: "--csv" :: Nil;
      } else {
        Nil;
      }) :::
      Nil;
    }

    def commandsEnable: Boolean = true;

  }

  private[this] def fileCompletion = new Completion {
    def isFilePath: Boolean = true;
    def parameters: List[String] = Nil;
    def options: List[String] = Nil;
    def commandsEnable: Boolean = false;
  }

}

object CommandSeqParserStatus {

  def init(inputType: CommandSeqInputType, outputType: CommandSeqOutputType) = CommandSeqParserStatus(
    inputType = inputType,
    outputType = outputType,
    inputFormat = None,
    inputFile = None,
    outputFile = None,
    inputFileArgIdx = 0,
    outputFileArgIdx = 0,
    commands = Vector.empty,
    lastCommand = NoneCommandParserStatus,
    childOrTailStatus = None,
  );

}

case class ParserErrorMessage(argIdx: Int, message: String);

case class HelpDocument(name: String);

case object SeqEnd;

case class CommandNodeSeq (
  inputType: CommandSeqInputType,
  outputType: CommandSeqOutputType,
  inputFormat: Option[InputFormat],
  inputFile: Option[String], // 空文字列は標準入力の意味
  outputFile: Option[String],
  inputFileArgIdx: Int,
  outputFileArgIdx: Int,
  commands: Vector[CommandNode],
) {
}

sealed trait InputFormat { def name: String }
case object TsvInputFormat extends InputFormat { def name = "tsv" }
case object CsvInputFormat extends InputFormat { def name = "csv" }

// CommandSeqInputType, CommandSeqOutputType について
//   サブコマンドごとの例
//     join, paste, union のパラメータとしてのサブコマンド
//       input:  標準入力または外部ファイルまたはjoinなどの入力をtee
//         inputType = SomeInputOrNone
//       output: joinなどへの入力
//         outputType = NoneOutput
//     diff のパラメータとしてのサブコマンド
//       input:  標準入力または外部ファイルまたはdiffの入力をtee
//         inputType = SomeInputOrNone
//       output: diffへの入力
//         outputType = NoneOutput
//     tee のパラメータとしてのサブコマンド
//       input:  teeの入力をtee
//         inputType = NoneInput
//       output: 標準出力または外部ファイル
//         outputType = SomeOutputOrNone
sealed trait CommandSeqInputType {
  def enableInputFileParam: Boolean;
}
object CommandSeqInputType {
  case object SomeInput extends CommandSeqInputType {
    def enableInputFileParam: Boolean = true;
  }
  case object SomeInputOrNone extends CommandSeqInputType {
    def enableInputFileParam: Boolean = true;
  }
  case object NoneInput extends CommandSeqInputType {
    def enableInputFileParam: Boolean = false;
  }
}
sealed trait CommandSeqOutputType {
  def enableOutputFileParam: Boolean;
}
object CommandSeqOutputType {
  case object SomeOutput extends CommandSeqOutputType {
    def enableOutputFileParam: Boolean = true;
  }
  case object SomeOutputOrNone extends CommandSeqOutputType {
    def enableOutputFileParam: Boolean = true;
  }
  case object NoneOutput extends CommandSeqOutputType {
    def enableOutputFileParam: Boolean = false;
  }
}

trait CommandParser {
  def initStatus(argIdx: Int): CommandParserStatus;
}

trait CommandParserStatus {

  def help: HelpDocument;

  def eatOption(opt: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    Option[(Either3[CommandParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)])];

  def eatArgument(arg: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    Option[(Either3[CommandParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)])];

  def childParser: Option[ChildCommandSeqParser];

  def tailParser: Option[TailCommandSeqParser];

  def finish: Either[ParserErrorMessage, CommandNode];

  def completion: Completion;

}

trait ChildCommandSeqParser {

  def initStatus: CommandSeqParserStatus;

  def endStatus(commands: CommandNodeSeq): CommandParserStatus;

}

trait TailCommandSeqParser {

  def initStatus: CommandSeqParserStatus;

  def endStatus(commands: CommandNodeSeq): CommandParserStatus;

}

case object NoneCommandParserStatus extends CommandParserStatus {

  def help: HelpDocument = {
    throw new AssertionError("TODO");
  }

  def eatOption(opt: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    Option[(Either3[CommandParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)])] = {
    None;
  }

  def eatArgument(arg: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    Option[(Either3[CommandParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)])] = {
    None;
  }

  def childParser: Option[ChildCommandSeqParser] = {
    None;
  }

  def tailParser: Option[TailCommandSeqParser] = {
    None;
  }

  def finish: Either[ParserErrorMessage, CommandNode] = throw new AssertionError();

  def completion: Completion = throw new AssertionError();

}

trait CommandNode {

  // 出力がTSV形式かどうか
  // TSV形式でない場合はこの後ろに次のコマンドを配置することができない
  //def isOutputTsv: Boolean;

  // CommandGraphにてノードとして扱うかどうか
  //def isCommandGraphNode: Boolean;

  // CommandGraphにてノードとして扱う場合に前後のコマンド列を含めてエッジとして追加する
  //def addNodeToGraph(graph: CommandGraph,
  //  prevCommands: Vector[CommandSeqNode], nextCommands: Vector[CommandSeqNode],
  //  inputEdgeId: Int, outputEdgeId: Int): CommandGraph;

}

