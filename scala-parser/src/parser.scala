// mulang-bin-sources: scala

object OptionParser {

  def parse(args: List[String], ctxt: OptionParserContext):
    Either[ParserMessage, OptionParserStatus] = {

    @scala.annotation.tailrec
    def sub(status: OptionParserStatus, args: List[String], argIdx: Int):
      Either[ParserMessage, OptionParserStatus] = {
      args match {
        case Nil =>
          Right(status);
        case arg :: tail =>
          status.eat(arg, tail, argIdx, ctxt) match {
            case (Left(msg: ParserMessage), _) =>
              Left(msg);
            case (Right(status), Some((args, argIdx))) =>
              sub(status, args, argIdx);
            case (Right(status), None) =>
              Right(status);
          }
      }
    }

    val status = OptionParserStatus.init(CommandSeqInputType.SomeInput, CommandSeqOutputType.SomeOutput);
    sub(status, args, 0);
  }

  def isOption(arg: String): Boolean = {
    arg.startsWith("-") || arg == "[";
  }

  val commands: Map[String, CommandParser] = Map (
    //"cut" -> CutCommandParser,
    //"diff" -> DiffCommandParser,
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
case class OptionParserStatus (
  inputType: CommandSeqInputType,
  outputType: CommandSeqOutputType,
  inputFormat: Option[InputFormat],
  inputFile: Option[String], // 空文字列は標準入力の意味
  outputFile: Option[String],
  inputFileArgIdx: Int,
  outputFileArgIdx: Int,
  commands: Vector[CommandNode],
  lastCommand: CommandParserStatus,
) {

  def eat(arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    (Either[ParserMessage, OptionParserStatus], Option[(List[String], Int)]) = {
    if (OptionParser.isOption(arg)) {
      if (arg == "--help") {
        // --help はすべてのコマンドで共通のオプションであるため最初にチェック
        (Left(lastCommand.help), None);
      } else {
        // コマンドのオプションとして解釈を試みる
        lastCommand.eatOption(arg, tail, argIdx, ctxt) match {
          case None =>
            // コマンドのオプションとして解釈できなかったケース
            // グローバルのオプションとして解釈を試みる
            eatOption(arg, tail, argIdx, ctxt);
          case Some((Right(cmd), opt)) =>
            // コマンドのオプションとして解釈できたケース
            (Right(this.copy(lastCommand = cmd)), opt);
          case Some((Left(err), opt)) =>
            // コマンドのオプションとして解釈できたけどエラーのケース
            (Left(err), opt);
        }
      }
    } else { // - 以外で始まる引数
      if (OptionParser.commandExists(arg)) {
        // コマンド名と同じ名前のファイルが偶然存在していたとしてもコマンド名を優先する。
        // 実行環境による解釈の揺れを少しでも減らすため。
        val nextCommand = OptionParser.command(arg, argIdx);
        if (lastCommand == NoneCommandParserStatus) {
          (Right(this.copy(lastCommand = nextCommand)), Some((tail, argIdx + 1)));
        } else {
          lastCommand.finish match {
            case Left(err) =>
              (Left(err), None);
            case Right(cmd) =>
              (Right(this.copy(commands = commands :+ cmd, lastCommand = nextCommand)), Some((tail, argIdx + 1)));
          }
        }
      } else if (ctxt.inputFileExists(arg) && inputFile.isEmpty) {
        (Right(this.copy(inputFile = Some(arg), inputFileArgIdx = argIdx)), Some((tail, argIdx + 1)));
      } else if (ctxt.inputFileExists(arg) && lastCommand == NoneCommandParserStatus) {
        (Left(ParserErrorMessage(argIdx, "duplicated parameter")), None);
      } else if (!ctxt.inputFileExists(arg) && lastCommand == NoneCommandParserStatus) {
        (Left(ParserErrorMessage(argIdx, "file not found")), None);
      } else {
        lastCommand.eatArgument(arg, tail, argIdx, ctxt) match {
          case Some((Right(cmd), opt)) =>
            (Right(this.copy(lastCommand = cmd)), opt);
          case Some((Left(err), opt)) =>
            (Left(err), opt);
          case None =>
            (Left(ParserErrorMessage(argIdx, "unknown argument")), None);
        }
      }
    }
  }

  private[this] def eatOption(opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    (Either[ParserMessage, OptionParserStatus], Option[(List[String], Int)]) = {
    if (opt == "--tsv") {
      inputFormat match {
        case Some(_) =>
          (Left(ParserErrorMessage(argIdx, "duplicated option")), None);
        case None =>
          (Right(this.copy(inputFormat = Some(TsvInputFormat))), Some((tail, argIdx + 1)));
      }
    } else if (opt == "--csv") {
      throw new AssertionError("TODO");
    } else if (opt == "-") {
      throw new AssertionError("TODO");
    } else if (opt == "-i") {
      throw new AssertionError("TODO");
    } else if (opt == "-o") {
      throw new AssertionError("TODO");
    } else {
      (Left(ParserErrorMessage(argIdx, "unknown option")), None);
    }
  }

  def completion(arg: String, argIdx: Int, ctxt: OptionParserContext):
    Option[OptionCompletion] = {
    throw new AssertionError("TODO");
  }

  def finish: Either[ParserErrorMessage, CommandNodeSeq] = {
    throw new AssertionError("TODO");
  }

}

object OptionParserStatus {

  def init(inputType: CommandSeqInputType, outputType: CommandSeqOutputType) = OptionParserStatus(
    inputType = inputType,
    outputType = outputType,
    inputFormat = None,
    inputFile = None,
    outputFile = None,
    inputFileArgIdx = 0,
    outputFileArgIdx = 0,
    commands = Vector.empty,
    lastCommand = NoneCommandParserStatus,
  );

}

sealed trait ParserMessage;
case class ParserErrorMessage(argIdx: Int, message: String) extends ParserMessage;
case class HelpDocument(name: String) extends ParserMessage;

trait OptionCompletion {
  def isFilePath: Boolean;
  def parameters: List[String];
  def options: List[String];
  def commandsEnable: Boolean;
}

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
sealed trait CommandSeqInputType;
object CommandSeqInputType {
  case object SomeInput extends CommandSeqInputType;
  case object SomeInputOrNone extends CommandSeqInputType;
  case object NoneInput extends CommandSeqInputType;
}
sealed trait CommandSeqOutputType;
object CommandSeqOutputType {
  case object SomeOutput extends CommandSeqOutputType;
  case object SomeOutputOrNone extends CommandSeqOutputType;
  case object NoneOutput extends CommandSeqOutputType;
}

sealed trait ChildCommandSeqType;
object ChildCommandSeqType {
  case object NormalChildCommandSeqType extends ChildCommandSeqType;
  case object TailChildCommandSeqType extends ChildCommandSeqType;
}

trait CommandParser {
  def initStatus(argIdx: Int): CommandParserStatus;
}

trait CommandParserStatus {

  def childParser: Option[(OptionParserStatus, ChildCommandSeqType)];

  def updateChildParser(childParser: OptionParserStatus): CommandParserStatus;

  def endChildParser(childCommands: CommandNodeSeq): CommandParserStatus;

  def help: HelpDocument;

  def eatOption(opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Option[(Either[ParserErrorMessage, CommandParserStatus], Option[(List[String], Int)])];

  def eatArgument(arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Option[(Either[ParserErrorMessage, CommandParserStatus], Option[(List[String], Int)])];

  def finish: Either[ParserErrorMessage, CommandNode];

}

case object NoneCommandParserStatus extends CommandParserStatus {

  def childParser: Option[(OptionParserStatus, ChildCommandSeqType)] = {
    None;
  }

  def updateChildParser(childParser: OptionParserStatus): CommandParserStatus = {
    throw new AssertionError();
  }

  def endChildParser(childCommands: CommandNodeSeq): CommandParserStatus = {
    throw new AssertionError();
  }

  def help: HelpDocument = {
    throw new AssertionError("TODO");
  }

  def eatOption(opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Option[(Either[ParserErrorMessage, CommandParserStatus], Option[(List[String], Int)])] = {
    None;
  }

  def eatArgument(arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Option[(Either[ParserErrorMessage, CommandParserStatus], Option[(List[String], Int)])] = {
    None;
  }

  def finish: Either[ParserErrorMessage, CommandNode] = throw new AssertionError();

}

trait CommandNode {

  // 出力がTSV形式かどうか
  // TSV形式でない場合はこの後ろに次のコマンドを配置することができない
  def isOutputTsv: Boolean;

  //// CommandGraphにてノードとして扱うかどうか
  //def isCommandGraphNode: Boolean;

  //// CommandGraphにてノードとして扱う場合に前後のコマンド列を含めてエッジとして追加する
  //def addNodeToGraph(graph: CommandGraph,
  //  prevCommands: Vector[CommandSeqNode], nextCommands: Vector[CommandSeqNode],
  //  inputEdgeId: Int, outputEdgeId: Int): CommandGraph;

  //def toTreeString: List[String];

}

