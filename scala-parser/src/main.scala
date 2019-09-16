// mulang-bin-sources: scala

import java.io.IOException;
import scala.concurrent.Await;
import scala.concurrent.Future;

object Main {

  def main(args: Array[String]): Unit = {
    val ctxt = parserContext();
    args.toList match {
      case Nil =>
        throw new Exception("command expected");
      case "exec" :: args =>
        OptionParser.parseCommands(args, ctxt) match {
          case Option3A(commands) =>
            execCommands(commands);
          case Option3B(err) =>
            error(err, args);
          case Option3C(help) =>
            execHelp(help);
        }
      case "complete" :: sh :: args =>
        OptionParser.parseCompletion(args, ctxt) match {
          case Some(completion) =>
            // シェル補完を実行
            sh match {
              case "bash" =>
                System.err.println(": TODO");
              case "zsh" =>
                println(Completion.toZshCompletion(completion).mkString("\n"));
            }
          case None =>
            // nop
        }
      case cmd :: _ =>
        throw new Exception("command not found: " + cmd);
    }
    System.exit(0);
  }

  private def parserContext() = new OptionParserContext {
    def inputFileExists(file: String): Boolean = {
      val f = new java.io.File(file);
      f.exists && f.isFile;
    }
  }

  private def error(e: ParserErrorMessage, args: List[String]): Unit = {
    val a = args(e.argIdx);
    System.err.println("error: (%d) \"%s\": %s".format(e.argIdx, a, e.message));
  }

  private def execHelp(help: HelpDocument): Unit = {
    System.err.println(help); // TODO
  }

  private def execCommands(commands: IndexedSeq[Graph.Node[CommandGraphNode]]): Unit = {
    val ps = ProcessBuilder.build(commands);
    ps.explain();
    //val future: Future[Unit] = ps.start();
    //Await.ready(future, scala.concurrent.duration.Duration.Inf);
  }

  def isOutputTty: Boolean = {
    terminalLines.nonEmpty && terminalCols.nonEmpty;
  }

  def terminalLines: Option[Int] = {
    Option(System.getenv("TERMINAL_LINES")) match {
      case None => None;
      case Some(s) =>
        try {
          Some(s.toInt);
        } catch {
          case _: NumberFormatException => None;
        }
    }
  }

  def terminalCols: Option[Int] = {
    Option(System.getenv("TERMINAL_COLS")) match {
      case None => None;
      case Some(s) =>
        try {
          Some(s.toInt);
        } catch {
          case _: NumberFormatException => None;
        }
    }
  }

  val sourceDir: String = {
    Option(System.getenv("MULANG_SOURCE_DIR")) match {
      case None => throw new IOException();
      case Some(s) => s;
    }
  }

  val softWorkingDir: String = {
    Option(System.getenv("MULANG_SOFT_WORKING_DIR")) match {
      case None => throw new IOException();
      case Some(s) => s;
    }
  }

}

