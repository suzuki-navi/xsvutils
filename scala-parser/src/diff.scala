// mulang-bin-sources: scala

import OptionParser.ForCommand._;

object DiffCommandParser extends CommandParser {
  def initStatus(lastCommand: CommandOptions, argIdx: Int): CommandParserStatus =
    DiffCommandParserStatus(lastCommand, argIdx, None, None);
}

case class DiffCommandParserStatus (
  prevCommand: CommandOptions,
  argIdx: Int,
  other: Option[CommandSeq],
  tail: Option[CommandSeq],
) extends CommandParserStatus {

  def parseOption(status: CommandSeqParserStatus,
    opt: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    Option[(Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)])] = {
    if (opt == "--file") {
      if (other.isEmpty) {
        tail match {
          case arg2 :: tail2 =>
            if (ctxt.inputFileExists(arg2)) {
              val other = CommandSeq.inputFile(arg2);
              Some((Right(status.copy(lastCommand = this.copy(other = Some(other)))),
                None, Some((tail2, argIdx + 2))));
            } else {
              Some((Left(ParserErrorMessage(argIdx + 1, "file not found")), None, None));
            }
          case Nil =>
            Some((Left(ParserErrorMessage(argIdx, "file path expected")),
              Some(completionFile), Some((tail, argIdx + 1))));
        }
      } else {
        Some((Left(ParserErrorMessage(argIdx, "duplicated option")), None, None));
      }
    } else if (opt == "[") {
      if (other.isEmpty) {
        val parentStatus = status;
        val receiver = new CommandSeqReceiver {
          def receive(status: CommandSeqParserStatus):
            (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion]) = {
            status.finish("diff", inputType = 1, outputType = 2) match {
              case Left(e) =>
                (Left(e), None);
              case Right(cmds) =>
                (Right(parentStatus.copy(
                  lastCommand = DiffCommandParserStatus.this.copy(other = Some(cmds)))), None);
            }
          }
        }
        Some((Right(CommandSeqParserStatus.init.copy(closeReceiver = Some(receiver))), None, Some((tail, argIdx + 1))));
      } else {
        Some((Left(ParserErrorMessage(argIdx, "duplicated option")), None, None));
      }
    } else {
      None;
    }
  }

  def parseArgument(status: CommandSeqParserStatus,
    arg: String, tail: List[String], argIdx: Int, ctxt: OptionParserContext):
    (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]) = {
    if (other.isEmpty) {
      if (ctxt.inputFileExists(arg)) {
        val other = CommandSeq.inputFile(arg);
        (Right(status.copy(lastCommand = this.copy(other = Some(other)))),
          None, Some((tail, argIdx)));
      } else {
        (Left(ParserErrorMessage(argIdx, "file not found")), None, None);
      }
    } else {
      (Left(ParserErrorMessage(argIdx, "unknown argument")), None, None);
    }
  }

  def help = new HelpDocument {
    def document: List[String] = List(
      "TODO",
    );
  }

  def completion = new Completion {
    def isFilePath: Boolean = true;
    def options: List[String] = Nil; // TODO
    def commandsEnable: Boolean = false; // TODO
  }

  private[this] def completionFile = new Completion {
    def isFilePath: Boolean = true;
    def options: List[String] = Nil;
    def commandsEnable: Boolean = false;
  }

  def finish(status: CommandSeqParserStatus):
    Either[CommandSeqReceiver, Either[ParserErrorMessage, CommandOptions]] = {
    other match {
      case None =>
        Right(Left(ParserErrorMessage(argIdx, "expected --file option")));
      case Some(other) =>
        tail match {
          case Some(tail) =>
            Right(Right(SomeCommandOptions(prevCommand, DiffCommandSeqNode(other, tail))));
          case None =>
            val parentStatus = status;
            val receiver = new CommandSeqReceiver {
              def receive(status: CommandSeqParserStatus):
                (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion]) = {
                status.finish("diff", inputType = 2, outputType = 2) match {
                  case Left(e) =>
                    (Left(e), None);
                  case Right(cmds) =>
                    (Right(parentStatus.copy(
                      lastCommand = DiffCommandParserStatus.this.copy(tail = Some(cmds)))), None);
                }
              }
            }
            Left(receiver);
        }
    }
  }

}

case class DiffCommandSeqNode (
  other: CommandSeq,
  tail: CommandSeq,
) extends CommandSeqNode {

  // 後続のコマンドをパラメータとして受け取るコマンドでは
  // このメソッドは呼び出されないはず
  def isOutputTsv: Boolean = throw new AssertionError();

  def isCommandGraphNode: Boolean = true;

  def addNodeToGraph(graph: CommandGraph,
    prevCommands: Vector[CommandSeqNode], nextCommands: Vector[CommandSeqNode],
    inputEdgeId: Int, outputEdgeId: Int): CommandGraph = {

    other.inputFile match {
      case None =>
        val  graph1 = graph;
        val (graph2, otherIn)  = graph1.addEdge;
        val (graph3, otherOut) = graph2.addEdge;
        val (graph4, prevOut)  = graph3.addEdge;
        val (graph5, diffIn)   = graph4.addEdge;
        val (graph6, resultIn) = graph5.addEdge;

        val  graph7            = graph6.addNode(
          TeeCommandGraphNode(prevOut, diffIn :: otherIn :: Nil));
        val  graph8            = graph7.addCommandSeq(other.commands, otherIn, otherOut);

        val  graph9            = graph8.addCommandSeq(prevCommands, inputEdgeId, prevOut);
        val  graph10           = graph9.addCommandSeq(nextCommands, resultIn, outputEdgeId);
        val  graph11           = graph10.addNode(
          DiffCommandGraphNode(diffIn, otherOut, resultIn));
        graph11;
      case Some(otherInputFile) =>
        val  graph1 = graph;
        val (graph2, otherIn)  = graph1.addEdge;
        val (graph3, otherOut) = graph2.addEdge;
        val (graph4, prevOut)  = graph3.addEdge;
        val  graph5 = graph4;
        val (graph6, resultIn) = graph5.addEdge;

        val  graph7            = graph6.addNode(
          FileInputCommandGraphNode(other.inputFormat, otherInputFile, otherIn));
        val  graph8            = graph7.addCommandSeq(other.commands, otherIn, otherOut);

        val  graph9            = graph8.addCommandSeq(prevCommands, inputEdgeId, prevOut);
        val  graph10           = graph9.addCommandSeq(nextCommands, resultIn, outputEdgeId);
        val  graph11           = graph10.addNode(
          DiffCommandGraphNode(prevOut, otherOut, resultIn));
        graph11;
    }
  }

  def toTreeString: List[String] = {
    "name: %s".format("diff") ::
    "other:" ::
    other.toTreeString.map("  " + _) :::
    "tail:" ::
    tail.toTreeString.map("  " + _) :::
    Nil;
  }

}

case class DiffCommandGraphNode (
  inputEdgeId: Int,
  otherEdgeId: Int,
  outputEdgeId: Int,
) extends CommandGraphNode {
  def inputs:  List[Int] = inputEdgeId :: otherEdgeId :: Nil;
  def outputs: List[Int] = outputEdgeId :: Nil;
}

