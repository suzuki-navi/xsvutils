// mulang-bin-sources: scala

object Main {

  def main(args: Array[String]): Unit = {
    val ctxt = parserContext();
    args.toList match {
      case Nil =>
        throw new Exception("command expected");
      case "exec" :: args =>
        OptionParser.parse(args, ctxt) match {
          case Right(status) =>
            execCommands(status);
          case Left(err: ParserErrorMessage) =>
            error(err, args);
          case Left(help: HelpDocument) =>
            execHelp(help);
        }
      case "complete" :: sh :: args =>
        val args2 = args.take(args.size - 1).toList;
        val last = args(args.size - 1);
        OptionParser.parse(args2, ctxt) match {
          case Right(status) =>
            status.completion(last, args.size - 1, ctxt) match {
              case Some(completion) =>
                // シェル補完を実行
                sh match {
                  case "bash" =>
                    System.err.println(": TODO");
                  case "zsh" =>
                    // println(Document.toZshCompletion(completion).mkString("\n"));
                }
              case None =>
                // nop
            }
          case Left(_) =>
            // --help オプションがあった場合
            // または、構文エラーの場合は
            // シェル補完をしない。
            // エラー出力してもシェルの表示が乱れるのでなにもしない。
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

  private def execCommands(parserStatus: CommandSeqParserStatus): Unit = {
    pprint.pprintln(parserStatus); // TODO
  }

}

