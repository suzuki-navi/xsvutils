// mulang-bin-sources: scala

import java.io.IOException;
import scala.concurrent.Await;
import scala.concurrent.Future;
import scala.concurrent.Promise;
import scala.concurrent.duration.Duration;

sealed trait CompressionType { def name: String }
object CompressionType {
  case object Gz extends CompressionType { def name = "gz" }
  case object Xz extends CompressionType { def name = "xz" }
}

sealed trait InputTableFormat { def name: String }
object InputTableFormat {
  case object Tsv extends InputTableFormat { def name = "tsv" }
  case object Csv extends InputTableFormat { def name = "csv" }
}

sealed trait NewlineType { def name: String }
object NewlineType {
  case object Unix extends NewlineType { def name = "unix" }
  case object Dos extends NewlineType { def name = "dos" }
  case object Mac extends NewlineType { def name = "mac" }
}

sealed trait Charencoding { def name: String }
object Charencoding {
  case object Utf8 extends Charencoding { def name = "utf8" }
  case object Utf8bom extends Charencoding { def name = "utf8-bom" }
  case object Sjis extends Charencoding { def name = "sjis" }
}

case class FileInputCommandGraphNode (
  tableFormat: Option[InputTableFormat],
  path: String, // 空文字列は標準入力の意味
) extends CommandGraphNode {

  private[this] val formatReaderFuture: Future[FormatReader.Result] = {
    FileInputCommandGraphNode.fileInputCount = FileInputCommandGraphNode.fileInputCount + 1;
    FormatReader.read(this, FileInputCommandGraphNode.fileInputCount);
  }

  def toProcessNode(node: Graph.Node[CommandGraphNode],
    newNexts: IndexedSeq[Graph.Edge[CommandGraphNode]]): Graph.Node[CommandGraphNode] = {
    val newNext = newNexts(0);
    val result: FormatReader.Result = Await.result(formatReaderFuture, Duration.Inf);
    val nodes = FileInputCommandGraphNode.toCommandGraphNodeSeq(result);
    val newEdge = nodes.tail.reverse.foldLeft[Graph.Edge[CommandGraphNode]](newNext) { (edge, node) =>
      val newNode = Graph.Node[CommandGraphNode](node, Vector(edge), 1);
      val newEdge = Graph.Edge[CommandGraphNode](newNode, 0);
      newEdge;
    }
    val newNode = Graph.Node[CommandGraphNode](nodes.head, Vector(newEdge), 0);
    newNode;
  }

  def toTask(inputs: IndexedSeq[FilePath], outputs: IndexedSeq[FilePath]): ProcessBuildingTask = {
    throw new AssertionError(); // ここにはこないはず
  }

}

object FileInputCommandGraphNode {

  private var fileInputCount: Int = 0;

  private def toCommandGraphNodeSeq(result: FormatReader.Result):
    List[CommandGraphNode] = {
    val newlineTypeNodes = Nil; // TODO
    val tableFormatNodes = Nil; // TODO
    val charencodingNodes = result.charencoding match {
      case Charencoding.Utf8 =>
        Nil;
      case Charencoding.Utf8bom =>
        BomTailCommandGraphNode() :: Nil;
      case Charencoding.Sjis =>
        IconvCommandGraphNode("cp932") :: Nil;
    }
    CatCommandGraphNode(Some(result.path), None) ::
      newlineTypeNodes ::: tableFormatNodes ::: charencodingNodes;
  }

}

object FormatReader {

  implicit val ec = ProcessUtil.executorContext;

  case class Result (
    path: FilePath,
    flagPath: WorkingFilePath,
    compressionType: List[CompressionType], // "gz", "xz"
    newlineType: NewlineType,  // "unix", "dos", "mac"
    tableFormat: InputTableFormat,    // tsv, csv
    charencoding: Charencoding, // "utf8", "utf8-bom", "sjis"
  );

  private val mulang_source_dir: String = {
    val d = System.getenv("MULANG_SOURCE_DIR");
    if (d == null) {
      throw new IOException();
    } else {
      d;
    }
  }

  def read(file: FileInputCommandGraphNode, id: Int): Future[Result] = {
    val path = file.path;
    Future[Result] {

      val pingPath = WorkingFilePath("input-" + id + "-ping.fifo");
      val flagPath = WorkingFilePath("input-" + id + "-flags.txt");
      val pipePath = WorkingFilePath("input-" + id + "-pipe.tsv");
      ProcessUtil.mkfifo(pingPath.path);
      ProcessUtil.mkfifo(pipePath.path);
      val future1: Future[Unit] = if (path.isEmpty) {
        val command = List(
          "perl",
          mulang_source_dir + "/input-format-detector.pl",
          "-f", flagPath.path,
          "-p", pingPath.path,
          "-o", pipePath.path);
        Future[Unit] {
          ProcessUtil.doProcess(command, ProcessUtil.Stdin, ProcessUtil.NullOutput);
        }
      } else {
        val command = List(
          "perl",
          mulang_source_dir + "/input-format-detector.pl",
          "-f", flagPath.path,
          "-p", pingPath.path,
          "-i", path,
          "-o", pipePath.path);
        Future[Unit] {
          ProcessUtil.doProcess(command, ProcessUtil.NullInput, ProcessUtil.NullOutput);
        }
      }

      val future2 = Future[Unit] {
        val fp1 = scala.io.Source.fromFile(pingPath.path);
        try {
          fp1.getLines.toList;
        } finally {
          fp1.close();
        }
      }

      val promise = Promise[Unit];
      future1.onComplete { t => promise.tryComplete(t) }
      future2.onComplete { t => promise.tryComplete(t) }
      Await.ready(promise.future, Duration.Inf);

      val fp = scala.io.Source.fromFile(flagPath.path);
      val lines = try {
        fp.getLines.toList;
      } finally {
        fp.close();
      }

      newResult(lines, file, pipePath, flagPath);
    }
  }

  private def newResult(lines: List[String], file: FileInputCommandGraphNode,
    pipePath: WorkingFilePath, flagPath: WorkingFilePath): Result = {
    val compressionType = lines.flatMap {
      case "gz" => CompressionType.Gz :: Nil;
      case "xz" => CompressionType.Xz :: Nil;
      case _ => Nil;
    }
    val newlineType = if (lines.contains("dos")) {
      NewlineType.Dos;
    } else if (lines.contains("mac")) {
      NewlineType.Mac;
    } else {
      NewlineType.Unix;
    }
    val tableFormat: InputTableFormat = file.tableFormat match {
      case Some(f) =>
        f;
      case None =>
        if (lines.contains("csv")) {
          InputTableFormat.Csv;
        } else {
          InputTableFormat.Tsv;
        }
    }
    val charencoding = if (lines.contains("utf8-bom")) {
      Charencoding.Utf8bom;
    } else if (lines.contains("sjis")) {
      Charencoding.Sjis;
    } else {
      Charencoding.Utf8;
    }
    val path = if (!lines.contains("pipe")) {
      UserFilePath(file.path);
    } else if (file.path.isEmpty) {
      StdinPipePath(pipePath.name, compressionType);
    } else {
      UserPipePath(file.path, pipePath.name, compressionType);
    }
    Result(path, flagPath,
      compressionType, newlineType, tableFormat, charencoding);
  }

}

