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
  private val working_dir: String = "./tmp"; // TODO

  def read(file: FileInputCommandGraphNode, id: Int): Future[Result] = {
    val path = file.path; // TODO 空文字列の場合
    Future[Result] {

      val pingPath = WorkingFilePath("input-" + id + "-ping.fifo");
      val flagPath = WorkingFilePath("input-" + id + "-flags.txt");
      val pipePath = WorkingFilePath("input-" + id + "-pipe.tsv");
      ProcessUtil.mkfifo(pingPath.path);
      ProcessUtil.mkfifo(pipePath.path);
      val command = List(
        "perl",
        mulang_source_dir + "/format-detector.pl",
        "-f", flagPath.path,
        "-p", pingPath.path,
        "-i", path,
        "-o", pipePath.path);

      val future1 = Future[Unit] {
        ProcessUtil.doProcess(command);
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
    val path = if (lines.contains("pipe")) {
      UserPipePath(file.path, pipePath.name, compressionType);
    } else {
      UserFilePath(file.path);
    }
    Result(path, flagPath,
      compressionType, newlineType, tableFormat, charencoding);
  }

}

