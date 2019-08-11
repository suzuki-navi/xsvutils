// mulang-bin-sources: scala

object CutCommandParser extends CommandParser {
  def initStatus(argIdx: Int): CommandParserStatus = {
    CutCommandParserStatus(argIdx, None);
  }
}

case class CutCommandParserStatus (
  argIdx: Int,
  cols: Option[List[String]],
) extends CommandParserStatus {

  def help: HelpDocument = throw new AssertionError("TODO");

  def eatOption(opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Option[(Either[ParserErrorMessage, CommandParserStatus], Option[(List[String], Int)])] = {
    if (opt == "--cols") {
      if (cols.isEmpty) {
        tail match {
          case arg :: tail2 =>
            Some((Right(this.copy(cols = Some(arg.split(",").toList))),
              Some(tail2, argIdx + 2)));
          case Nil =>
            Some((Left(ParserErrorMessage(argIdx, "cols expected")), None));
        }
      } else {
        Some((Left(ParserErrorMessage(argIdx, "duplicated option")), None));
      }
    } else {
      None;
    }
  }

  def eatArgument(arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Option[(Either[ParserErrorMessage, CommandParserStatus], Option[(List[String], Int)])] = {
    if (cols.isEmpty) {
      Some((Right(this.copy(cols = Some(arg.split(",").toList))), Some(tail, argIdx + 1)));
    } else {
      Some((Left(ParserErrorMessage(argIdx, "unknown argument")), None));
    }
  }

  def childParser: Option[ChildCommandSeqParser] = None;

  def tailParser: Option[TailCommandSeqParser] = None;

  def finish: Either[ParserErrorMessage, CommandNode] = {
    cols match {
      case None =>
        Left(ParserErrorMessage(argIdx, "expected --cols option"));
      case Some(cols) =>
        Right(CutCommandNode(cols));
    }
  }

}

case class CutCommandNode (
  cols: List[String],
) extends CommandNode {

}

