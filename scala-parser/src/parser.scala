// mulang-bin-sources: scala

object OptionParser {

  def parse(args: List[String], ctxt: OptionParserContext):
    Either[ParserMessage, OptionParserStatus] = {
    throw new AssertionError("TODO");
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
  //lastCommand: CommandParserStatus,
) {

  def eat(args: List[String], argIdx: Int, ctxt: OptionParserContext):
    (Either[ParserMessage, OptionParserStatus], Option[(List[String], Int)]) = {
    throw new AssertionError("TODO");
  }

  def completion(arg: String, argIdx: Int, ctxt: OptionParserContext):
    Option[OptionCompletion] = {
    throw new AssertionError("TODO");
  }

}

object OptionParserStatus {
  def init(inputType: CommandSeqInputType, outputType: CommandSeqOutputType): OptionParserStatus = {
    throw new AssertionError("TODO");
  }
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
  case object SomeInput;
  case object SomeInputOrNone;
  case object NoneInput;
}
sealed trait CommandSeqOutputType;
object CommandSeqOutputType {
  case object SomeOutput;
  case object SomeOutputOrNone;
  case object NoneOutput;
}

