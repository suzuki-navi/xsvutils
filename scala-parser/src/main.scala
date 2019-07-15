// mulang-bin-sources: scala

object Main {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case Nil =>
        throw new Exception("command expected");
      case "main" :: args =>
        OptionParser.parse(args, 0) match {
          case Right(status) =>
            println(status);
          case Left(help: OptionParser.HelpDocument) =>
            println(help.document);
          case Left(OptionParser.ParserErrorMessage(argIdx, message)) =>
            System.err.println("error: (%d) \"%s\": %s".format(argIdx, args(argIdx), message));
        }
      case "complete" :: sh :: args =>
        val args2 = args.take(args.size - 1).toList ::: "//help" :: Nil;
        OptionParser.parse(args2, 0) match {
          case Right(status) =>
            throw new AssertionError();
          case Left(help: OptionParser.HelpDocument) =>
            sh match {
              case "bash" =>
                println(": TODO");
              case "zsh" =>
                println(Document.toZshCompletion(help).mkString("\n"));
            }
          case Left(OptionParser.ParserErrorMessage(argIdx, message)) =>
            System.err.println("error: (%d) \"%s\": %s".format(argIdx, args2(argIdx), message));
        }
      case cmd :: _ =>
        throw new Exception("command not found: " + cmd);
    }
  }
}

