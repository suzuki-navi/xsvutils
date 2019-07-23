// mulang-bin-sources: scala

case class CommandSeq (
  inputFormat: Option[CommandSeq.InputFormat],
  inputFile: Option[String], // 空文字列は標準入力の意味、Noneはjoinなどの入力を引き継ぐ意味
  outputFile: Option[String], // 空文字列は標準出力の意味、Noneはjoinなどへの出力の意味
  commands: Vector[CommandSeq.CommandNode]) {

  def toTreeString: List[String] = {
    "inputFormat: %s".format(inputFormat.map(_.name).getOrElse("auto")) ::
    "inputFile: %s".format(inputFile.getOrElse("->")) ::
    "outputFile: %s".format(outputFile.getOrElse("->")) ::
    "commands: " ::
    (commands.toList.flatMap { cmd =>
      cmd.toTreeString match {
        case h :: t => "  - %s".format(h) :: t.map("    " + _);
        case Nil => "  - " :: Nil;
      }
    });
  }

}

object CommandSeq {

  trait CommandNode {

    def toTreeString: List[String];

  }

  sealed trait InputFormat { def name: String }
  case object TsvInputFormat extends InputFormat { def name = "tsv" }
  case object CsvInputFormat extends InputFormat { def name = "csv" }

  def inputFile(file: String) = CommandSeq (
    inputFormat = None,
    inputFile = Some(file),
    outputFile = None,
    commands = Vector.empty,
  );

  def apply(
    inputFormat: Option[InputFormat],
    inputFile: Option[String],
    outputFile: Option[String],
    lastCommand: OptionParser.CommandOptions,
  ): CommandSeq = {
    @scala.annotation.tailrec
    def sub(cmd: OptionParser.CommandOptions, seq: List[CommandNode]): CommandSeq = {
      cmd match {
        case OptionParser.NoneCommandOptions =>
          CommandSeq(
            inputFormat,
            inputFile,
            outputFile,
            seq.toVector);
        case cmd: OptionParser.SomeCommandOptions =>
          sub(cmd.prevCommand, cmd.command :: seq);
      }
    }
    sub(lastCommand, Nil);
  }

}

