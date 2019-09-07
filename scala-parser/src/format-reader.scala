// mulang-bin-sources: scala

import java.io.IOException;
import scala.concurrent.Await;
import scala.concurrent.Future;
import scala.concurrent.Promise;
import scala.concurrent.duration.Duration;
import scala.concurrent.duration.MILLISECONDS;

object FormatReader {

  import CommandGraph.FileInputCommandGraphNode;

  implicit val ec = ProcessUtil.executorContext;

  def read(files: Vector[FileInputCommandGraphNode]): Vector[Result] = {
    val futureList = files.zipWithIndex.map { case (file, index) =>
      formatReaderFuture(file, index + 1);
    }
    val results = futureList.map { f =>
      Await.result(f, Duration.Inf);
    }
    results;
  }

  case class Result (
    path: String,
    compressionType: List[String], // "gz", "xz"
    newlineType: String,  // "unix", "dos", "mac"
    tableType: InputTableFormat,    // tsv, csv
    charencoding: String, // "utf8", "utf8-bom", "sjis"
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

  private def formatReaderFuture(file: FileInputCommandGraphNode, id: Int): Future[Result] = {
    val path = file.path;
    Future[Result] {

      val pingPath = working_dir + "/input-" + id + "-ping.fifo";
      val flagPath = working_dir + "/input-" + id + "-flags.txt";
      val pipePath = working_dir + "/input-" + id + "-pipe.txt";
      ProcessUtil.mkfifo(pingPath);
      ProcessUtil.mkfifo(pipePath);
      val command = List(
        "perl",
        mulang_source_dir + "/format-detector.pl",
        "-f", flagPath,
        "-p", pingPath,
        "-i", path,
        "-o", pipePath);

      val future1 = Future[Unit] {
        ProcessUtil.doProcess(command);
      }

      val future2 = Future[Unit] {
        val fp1 = scala.io.Source.fromFile(pingPath);
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

      val fp = scala.io.Source.fromFile(flagPath);
      val lines = try {
        fp.getLines.toList;
      } finally {
        fp.close();
      }

      newResult(lines, file, pipePath);
    }
  }

  private def newResult(lines: List[String],
    file: FileInputCommandGraphNode, pipePath: String): Result = {
    val path = if (lines.contains("pipe")) {
      pipePath;
    } else {
      file.path;
    }
    val compressionType = lines.flatMap {
      case "gz" => "gz" :: Nil;
      case "xz" => "xz" :: Nil;
      case _ => Nil;
    }
    val newlineType = if (lines.contains("dos")) {
      "dos";
    } else if (lines.contains("mac")) {
      "mac";
    } else {
      "unix";
    }
    val tableType = file.format match {
      case Some(f) => f;
      case None =>
        if (lines.contains("csv")) {
          CsvInputTableFormat;
        } else {
          TsvInputTableFormat;
        }
    }
    val charencoding = if (lines.contains("utf8-bom")) {
      "utf8-bom";
    } else if (lines.contains("sjis")) {
      "sjis";
    } else {
      "utf8";
    }
    Result(path, compressionType, newlineType, tableType, charencoding);
  }

}

