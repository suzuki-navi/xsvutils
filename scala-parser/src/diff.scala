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

  def help: HelpDocument = HelpDocument("diff");

  def eatOption(opt: String, tail: List[String], argIdx: Int, isCompletion: Boolean,
    ctxt: OptionParserContext):
    Option[(Either3[CommandParserStatus, ParserErrorMessage, Completion], Option[(List[String], Int)])] = {
    if (opt == "[") {
      if (other.isEmpty) {
        val childStatus = CommandSeqParserStatus.init(
          CommandSeqInputType.SomeInputOrNone, CommandSeqOutputType.NoneOutput);
        Some((Option3A(this.copy(other = Some(Left(childStatus))))), Some((tail, argIdx + 1)));
      } else {
        Some((Option3B(ParserErrorMessage(argIdx, "duplicated option")), None));
      }
    } else if (opt == "--file" || opt == "--other") {
      if (other.isEmpty) {
        tail match {
          case arg :: Nil if isCompletion =>
            Some((Option3C(Completion.files), None));
          case arg :: tail2 =>
            if (ctxt.inputFileExists(arg)) {
              val other = CommandNodeSeq.inputFile(arg, argIdx);
              Some((Option3A(this.copy(other = Some(Right(other)))), Some(tail2, argIdx + 2)));
            } else {
              Some((Option3B(ParserErrorMessage(argIdx + 1, "file not found")), None));
            }
          case Nil =>
            Some((Option3B(ParserErrorMessage(argIdx, "file path expected")),
              Some((tail, argIdx + 1))));
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
    throw new AssertionError("TODO");
  }

  def eatTail: Option[CommandParserStatus] = {
    other match {
      case None => None; // このあとのfinishでエラーの扱いとなるはず
      case Some(Left(_)) =>
        throw new AssertionError(); // ここにはこないはず
      case Some(Right(otherCommands)) =>
        tail match {
          case Some(_) =>
            throw new AssertionError(); // ここにはこないはず
          case None => None;
            val tailStatus = CommandSeqParserStatus.init(
              CommandSeqInputType.NoneInput, CommandSeqOutputType.NoneOutput);
            Some(this.copy(tail = Some(Left(tailStatus))));
        }
    }
  }

  def childOrTailParser: Option[ChildOrTailCommandSeqParser] = {
    (other, tail) match {
      case (None, _) =>
        None;
      case (Some(Left(childStatus)), _) =>
        Some(new ChildOrTailCommandSeqParser {
          def isTail: Boolean = false;
          def status: CommandSeqParserStatus = childStatus;
          def updateStatus(status: CommandSeqParserStatus): CommandParserStatus = {
            DiffCommandParserStatus.this.copy(other = Some(Left(status)));
          }
          def endStatus(commands: CommandNodeSeq): CommandParserStatus = {
            DiffCommandParserStatus.this.copy(other = Some(Right(commands)));
          }
        });
      case (Some(Right(_)), None) =>
        None;
      case (Some(Right(_)), Some(Right(_))) =>
        None;
      case (Some(Right(_)), Some(Left(tailStatus))) =>
        Some(new ChildOrTailCommandSeqParser {
          def isTail: Boolean = true;
          def status: CommandSeqParserStatus = tailStatus;
          def updateStatus(status: CommandSeqParserStatus): CommandParserStatus = {
            DiffCommandParserStatus.this.copy(tail = Some(Left(status)));
          }
          def endStatus(commands: CommandNodeSeq): CommandParserStatus = {
            DiffCommandParserStatus.this.copy(tail = Some(Right(commands)));
          }
        });
    }
  }

  def finish: Either[ParserErrorMessage, CommandPipeNode] = {
    try {
      val newOther: CommandNodeSeq = other match {
        case None =>
          throw ParserErrorMessage(argIdx, "option `--other` expected").toException;
        case Some(Left(status)) =>
          status.finish match {
            case Left(err) =>
              throw err.toException;
            case Right(cmds) =>
              cmds;
          }
        case Some(Right(cmds)) =>
          cmds;
      }
      val newTail: Vector[CommandPipeNode] = tail match {
        case None =>
          Vector.empty;
        case Some(Left(status)) =>
          status.finish match {
            case Left(err) =>
              throw err.toException;
            case Right(cmds) =>
              cmds.commands;
          }
        case Some(Right(cmds)) =>
          cmds.commands;
      }
      Right(DiffCommandNode(newOther, newTail));
    } catch {
      case ParserException(msg) =>
        Left(msg);
    }
  }

  def completion = new Completion {

    def isFilePath: Boolean = other.isEmpty;

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
  other: CommandNodeSeq,
  tail: Vector[CommandPipeNode],
) extends CommandPipeNode {

  def isCommandGraphNode: Boolean = true;

  def addNodeToGraph(output: Graph.Edge[CommandNode]):
    (Graph.Edge[CommandNode], IndexedSeq[Graph.Node[CommandNode]]) = {
    other.inputFile match {
      case None =>
        val inputEdges = Graph.unshiftEdges(this, Vector(output), 2);
        val (input1Edge, nodes1) = CommandGraph.unshiftCommands(tail, inputEdges(0));
        val (input2Edge, nodes2) = CommandGraph.unshiftCommands(other.commands ++ tail, inputEdges(1));
        val teeInputEdge = Graph.unshiftEdges(TeeCommandNode(2), Vector(input1Edge, input2Edge), 1)(0);
        (teeInputEdge, nodes1 ++ nodes2);
    throw new AssertionError("作りかけ");
      case Some(inputFile) =>
        val inputEdges = Graph.unshiftEdges(this, Vector(output), 2);
        val (input1Edge, nodes1) = CommandGraph.unshiftCommands(tail, inputEdges(0));
        val (input2Edge, nodes2) = CommandGraph.unshiftCommands(other.commands ++ tail, inputEdges(1));
        val otherInputNode = CommandGraph.unshift(FileInputCommandNode(other.inputFormat, inputFile), input2Edge);
        (input1Edge, (nodes1 :+ otherInputNode) ++ nodes2);
    }
  }

}

