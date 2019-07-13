// mulang-bin-sources: scala

object Main {
  def main(args: Array[String]): Unit = {
    args.toList match {
      case Nil =>
        throw new Exception("command expected");
      case "main" :: tail =>
        OptionParser.parse(tail, 0) match {
          case Right(status) =>
            println(status);
          case Left(OptionParser.ParserErrorMessage(argIdx, message)) =>
            println("error: (%d) \"%s\": %s".format(argIdx, tail(argIdx), message));
        }
      case cmd :: tail =>
        throw new Exception("command not found: " + cmd);
    }
  }
}

