// mulang-bin-sources: main-jvm

object Main {

  def main(args: Array[String]): Unit = {
    args.toList match {
      case Nil =>
        throw new Exception("command expected");
      case "parser" :: _ =>
        throw new Exception("parser not implemented");
      case "completion" :: _ =>
        throw new Exception("completion not implemented");
      case cmd :: _ =>
        throw new Exception("command not found: " + cmd);
    }
    System.exit(0);
  }

}

