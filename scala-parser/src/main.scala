// mulang-bin-sources: scala

object Main {

  import OptionParser.OptionParserContext;
  import OptionParser.ParserErrorMessage;
  import OptionParser.CommandSeqOptions;
  import OptionParser.parseTotal;

  def main(args: Array[String]): Unit = {
    args.toList match {
      case Nil =>
        throw new Exception("command expected");
      case "main" :: args =>
        parseTotal(args, 0, parserContext()) match {
          case Right((status, false)) =>
            // 正常に実行
            status.finish match {
              case Left(e) =>
                error(e, args);
              case Right(cmds) =>
                execCmds(cmds);
            }
          case Right((status, true)) =>
            // --help オプションがあった場合はヘルプを表示
            println(status.help.document.mkString("\n"));
          case Left(e) =>
            // 構文エラー
            error(e, args);
        }
      case "complete" :: sh :: args =>
        val args2 = args.take(args.size - 1).toList;
        parseTotal(args2, 0, parserContext()) match {
          case Right((status, false)) =>
            // シェル補完を実行
            sh match {
              case "bash" =>
                System.err.println(": TODO");
              case "zsh" =>
                println(Document.toZshCompletion(status.completion).mkString("\n"));
            }
          case Right((status, true)) =>
            // --help オプションがあった場合はシェル補完をしない
          case Left(e) =>
            // 構文エラー
            // エラー出力してもシェルの表示が乱れるのでなにもしない
            //error(e, args);
        }
      case cmd :: _ =>
        throw new Exception("command not found: " + cmd);
    }
  }

  private def execCmds(opts: CommandSeqOptions): Unit = {
    val cmdseq = opts.toCommandSeq;
    System.err.println(cmdseq.toTreeString.map("# " + _).mkString("\n"));
  }

  private def parserContext() = new OptionParserContext {
    def inputFileExists(file: String): Boolean = {
      val f = new java.io.File(file);
      f.exists && f.isFile;
    }
  }

  private def error(e: ParserErrorMessage, args: List[String]): Unit = {
    val a = if (e.argIdx >= args.size) "" else args(e.argIdx);
    System.err.println("error: (%d) \"%s\": %s".format(e.argIdx, a, e.message));
  }

}

