// mulang-bin-sources: scala

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

  private def execCommands(commands: CommandNodeSeq): Unit = {
    pprint.pprintln(commands); // TODO
  }

}

