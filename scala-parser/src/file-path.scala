// mulang-bin-sources: scala

case class MkfifoInfo (
  path: WorkingFilePath,
) {

  def message: String = ("$ mkfifo " + path.debugStr);

  def run(): Unit = {
    ProcessUtil.mkfifo(path.path);
  }

}

trait FilePath {
  def path: String;
  def debugStr: String;
  def taskMessages: List[String];
}

case class UserFilePath (
  path: String,
) extends FilePath {
  def debugStr: String = path;
  def taskMessages: List[String] = Nil;
}

case class SourceFilePath (
  name: String,
) extends FilePath {
  def path: String = "./src/" + name; // TODO
  def debugStr: String = "./src/" + name; // TODO
  def taskMessages: List[String] = Nil;
}

case class WorkingFilePath (
  name: String,
) extends FilePath {
  def path: String = "./tmp/" + name; // TODO
  def debugStr: String = "./tmp/" + name; // TODO
  def taskMessages: List[String] = Nil;
}

case class UserPipePath (
  origPath: String,
  pipeName: String,
  compressionType: List[CompressionType],
) extends FilePath {
  def path: String = "./tmp/" + pipeName; // TODO
  def debugStr: String = "./tmp/" + pipeName; // TODO
  def taskMessages: List[String] = {
    MkfifoInfo(WorkingFilePath(pipeName)).message ::
    ("$ cat " + origPath + " \\") ::
    (compressionType.map {
      case CompressionType.Gz => "  | gunzip -c \\"
      case CompressionType.Xz => "  | xz -c -d \\"
    }).toList :::
    ("  > " + debugStr) ::
    Nil;
  }
}

case class StdinPipePath (
  pipeName: String,
  compressionType: List[CompressionType],
) extends FilePath {
  def path: String = "./tmp/" + pipeName; // TODO
  def debugStr: String = "$stdin";
  def taskMessages: List[String] = UserPipePath("$stdin", pipeName, compressionType).taskMessages;
}

case object StdoutPipePath extends FilePath {
  def path: String = "/dev/stdout";
  def debugStr: String = "$stdout";
  def taskMessages: List[String] = Nil;
}

case object NullPath extends FilePath {
  def path: String = "/dev/null";
  def debugStr: String = "$null";
  def taskMessages: List[String] = Nil;
}

