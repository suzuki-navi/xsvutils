// mulang-bin-sources: scala

object Main {

  import OptionParser.OptionParserContext;
  import OptionParser.ParserErrorMessage;
  import OptionParser.HelpDocument;
  import OptionParser.parse;

  def main(args: Array[String]): Unit = {
    args.toList match {
      case Nil =>
        throw new Exception("command expected");
      case "main" :: args =>
        parse(args, parserContext()) match {
          case (Left(e: ParserErrorMessage), _) =>
            error(e,args);
          case (Right(cmds), _) =>
            execCmds(cmds);
          case (Left(help: HelpDocument), _) =>
            println(help.document.mkString("\n"));
        }
      case "complete" :: sh :: args =>
        val args2 = args.take(args.size - 1).toList;
        parse(args2, parserContext()) match {
          case (_, Some(completion)) =>
            // シェル補完を実行
            sh match {
              case "bash" =>
                System.err.println(": TODO");
              case "zsh" =>
                println(Document.toZshCompletion(completion).mkString("\n"));
            }
          case (_, None) =>
            // --help オプションがあった場合
            // または、構文エラーの場合は
            // シェル補完をしない。
            // エラー出力してもシェルの表示が乱れるのでなにもしない。
        }
      case cmd :: _ =>
        throw new Exception("command not found: " + cmd);
    }
  }

  private def execCmds(commands: CommandSeq): Unit = {
    System.err.println(commands.toTreeString.map("# " + _).mkString("\n"));
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

