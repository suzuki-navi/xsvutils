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

  def finish: Either[ParserErrorMessage, CommandNode] = {
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
      val newTail: Vector[CommandNode] = tail match {
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
  tail: Vector[CommandNode],
) extends CommandNode {

  def isCommandGraphNode: Boolean = true;

  def addNodeToGraph(graph: CommandGraph,
    prevCommands: Vector[CommandNode], nextCommands: Vector[CommandNode],
    inputEdgeId: Int): (CommandGraph, Int) = {
    assert(nextCommands.isEmpty);
    other.inputFile match {
      case None =>
        val  graph0 = graph;
        val (graph1, input1Edge1)  = graph0.addCommandSeq(prevCommands, inputEdgeId);
        val (graph2, input1Edge2)  = graph1.addEdge;
        val (graph3, input2Edge1)  = graph2.addEdge;
        val  graph4                = graph3.addNode(TeeCommandGraphNode(input1Edge1, input1Edge2, input2Edge1));
        val (graph5, input1Edge3)  = graph4.addCommandSeq(nextCommands, input1Edge2);
        val (graph6, input2Edge2)  = graph5.addCommandSeq(nextCommands, input2Edge1);
        val (graph7, outputEdgeId) = graph6.addEdge;
        val  graph8                = graph7.addNode(DiffCommandGraphNode(input1Edge3, input2Edge2, outputEdgeId));
        (graph8, outputEdgeId);
      case Some(inputFile) =>
        val  graph0 = graph;
        val (graph1, input1Edge1)  = graph0.addCommandSeq(prevCommands ++ tail, inputEdgeId);
        val (graph2, input2Edge1)  = graph1.addFileInput(other.inputFormat, inputFile);
        val (graph3, input2Edge2)  = graph2.addCommandSeq(other.commands ++ tail, input2Edge1);
        val (graph4, outputEdgeId) = graph3.addEdge;
        val  graph5                = graph4.addNode(DiffCommandGraphNode(input1Edge1, input2Edge2, outputEdgeId));
        (graph5, outputEdgeId);
    }
  }

}

case class DiffCommandGraphNode(
  input1: Int,
  input2: Int,
  output: Int,
) extends CommandGraphNode {
  def inputs:  List[Int] = input1 :: input2 :: Nil;
  def outputs: List[Int] = output :: Nil;
}

