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
    (Either[ParserErrorMessage, CommandSeqParserStatus], Option[Completion], Option[(List[String], Int)]) = {
    if (opt == "--file") {
      if (other.isEmpty) {
        tail match {
          case arg2 :: tail2 =>
            if (ctxt.inputFileExists(arg2)) {
              val other = CommandSeq.inputFile(arg2);
              (Right(status.copy(lastCommand = this.copy(other = Some(other)))),
                None, Some((tail2, argIdx + 2)));
            } else {
              (Left(ParserErrorMessage(argIdx + 1, "file not found")), None, None);
            }
          case Nil =>
            (Left(ParserErrorMessage(argIdx, "file path expected")),
              Some(completionFile), Some((tail, argIdx + 1)));
        }
      } else {
        (Left(ParserErrorMessage(argIdx, "duplicated option")), None, None);
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
        (Right(CommandSeqParserStatus.init.copy(closeReceiver = Some(receiver))), None, Some((tail, argIdx + 1)));
      } else {
        (Left(ParserErrorMessage(argIdx, "duplicated option")), None, None);
      }
    } else {
      (Left(ParserErrorMessage(argIdx, "unknown option")), None, None);
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
            Right(Right(SomeCommandOptions(prevCommand, DiffCommandNode(other, tail))));
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

case class DiffCommandNode (
  other: CommandSeq,
  tail: CommandSeq,
) extends CommandNode {

  def toTreeString: List[String] = {
    "name: %s".format("diff") ::
    "other:" ::
    other.toTreeString.map("  " + _) :::
    "tail:" ::
    tail.toTreeString.map("  " + _) :::
    Nil;
  }

}

