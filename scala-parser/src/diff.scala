// mulang-bin-sources: scala

object DiffCommandParser extends CommandParser {
  def initStatus(argIdx: Int): CommandParserStatus = {
    DiffCommandParserStatus(argIdx, None, None);
  }
}

case class DiffCommandParserStatus (
  argIdx: Int,
  other: Option[Either[CommandSeqParserStatus, CommandNodeSeq]],
  tail: Option[Either[CommandSeqParserStatus, CommandNodeSeq]],
) extends CommandParserStatus {

  def help: HelpDocument = throw new AssertionError("TODO");

  def eatOption(opt: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    Option[(Either3[CommandParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)])] = {
    if (opt == "--file") {
      if (other.isEmpty) {
        tail match {
          case arg :: tail2 =>
            if (ctxt.inputFileExists(arg)) {
//              val other = CommandSeq.inputFile(arg);
//              Some((Right(status.copy(lastCommand = this.copy(other = Some(other)))),
//                Some((tail2, argIdx + 2))));
            } else {
              Some((Option3B(ParserErrorMessage(argIdx + 1, "file not found")), None));
            }
          case Nil =>
            Some((Option3B(ParserErrorMessage(argIdx, "file path expected")),
              Some((tail, argIdx + 1))));
        }
      } else {
      }
    } else {
    }
    throw new AssertionError("TODO");
  }

  def eatArgument(arg: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    Option[(Either3[CommandParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)])] = {
    throw new AssertionError("TODO");
  }

  def childParser: Option[ChildCommandSeqParser] = throw new AssertionError("TODO");

  def tailParser: Option[TailCommandSeqParser] = throw new AssertionError("TODO");

  def finish: Either[ParserErrorMessage, CommandNode] = {
    throw new AssertionError("TODO");
  }

  def completion = new Completion {
    def isFilePath: Boolean = true;
    def parameters: List[String] = Nil;
    def options: List[String] = {
      if (other.isEmpty) {
        "--other" :: "[" :: Nil;
      } else {
        Nil;
      }
    }
    def commandsEnable: Boolean = {
      finish match {
        case Left(_) => false;
        case Right(_) => true;
      }
    }
  }

}

case class DiffCommandNode (
) extends CommandNode {

}

