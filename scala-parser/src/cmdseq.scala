// mulang-bin-sources: scala

case class CommandSeq (
  inputFormat: Option[CommandSeq.InputFormat],
  inputFile: Option[String], // 空文字列は標準入力の意味、Noneはjoinなどの入力を引き継ぐ意味
  outputFile: Option[String], // 空文字列は標準出力の意味、Noneはjoinなどへの出力の意味
  commands: Vector[CommandSeq.CommandNode]) {

  def toTreeString: List[String] = {
    "inputFormat: %s".format(inputFormat.map(_.name).getOrElse("")) ::
    "inputFile: %s".format(inputFile.getOrElse("")) ::
    "outputFile: %s".format(outputFile.getOrElse("")) ::
    "commands: " ::
    commands.toList.map(cmd => "  - %s".format(cmd.toString)) :::
    Nil;
  }

}

object CommandSeq {

  trait CommandNode {
  }

  sealed trait InputFormat { def name: String }
  case object TsvInputFormat extends InputFormat { def name = "tsv" }
  case object CsvInputFormat extends InputFormat { def name = "csv" }

}

