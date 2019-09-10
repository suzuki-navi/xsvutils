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

  def help: HelpDocument = HelpDocument("cut");

  def eatOption(opt: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    Option[(Either3[CommandParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)])] = {
    if (opt == "--cols") {
      if (cols.isEmpty) {
        tail match {
          case arg :: Nil if isCompletion =>
            Some((Option3C(colsCompletion), None));
          case arg :: tail2 =>
            Some((Option3A(this.copy(cols = Some(arg.split(",").toList))),
              Some(tail2, argIdx + 2)));
          case Nil =>
            Some((Option3B(ParserErrorMessage(argIdx, "cols expected")), None));
        }
      } else {
        Some((Option3B(ParserErrorMessage(argIdx, "duplicated option")), None));
      }
    } else {
      None;
    }
  }

  def eatArgument(arg: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    Option[(Either3[CommandParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)])] = {
    if (cols.isEmpty) {
      Some((Option3A(this.copy(cols = Some(arg.split(",").toList))), Some(tail, argIdx + 1)));
    } else {
      Some((Option3B(ParserErrorMessage(argIdx, "unknown argument")), None));
    }
  }

  def eatTail: Option[CommandParserStatus] = None;

  def childOrTailParser: Option[ChildOrTailCommandSeqParser] = None;

  def finish: Either[ParserErrorMessage, CommandPipeNode] = {
    cols match {
      case None =>
        Left(ParserErrorMessage(argIdx, "option `--cols` expected"));
      case Some(cols) =>
        Right(CutCommandNode(cols));
    }
  }

  def completion = new Completion {
    def isFilePath: Boolean = false;
    def parameters: List[String] = Nil;
    def options: List[String] = {
      if (cols.isEmpty) {
        "--cols" :: Nil;
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

  private[this] def colsCompletion = new Completion {
    def isFilePath: Boolean = false;
    def parameters: List[String] = Nil;
    def options: List[String] = Nil;
    def commandsEnable: Boolean = false;
  }

}

case class CutCommandNode (
  cols: List[String],
) extends CommandPipeNode with CommandGraphNode {

  def addNodeToGraph(output: Graph.Edge[CommandGraphNode]):
    (Graph.Edge[CommandGraphNode], IndexedSeq[Graph.Node[CommandGraphNode]]) = {
    (Graph.unshiftEdges(this, Vector(output), 1)(0), Vector.empty);
  }

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] =
    toProcessNodeDefault(node, newNexts);

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    ForkProcessBuildingTask(Left("todo-cut") :: Nil, Some(inputs(0)), Some(outputs(0))); // TODO
  }

}

