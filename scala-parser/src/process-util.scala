// mulang-bin-sources: scala

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.Executors;
import scala.sys.process.Process;
import scala.concurrent.ExecutionContext;

object ProcessUtil {

  def doProcess(command: List[String], input: Input, output: Output): Unit = {
    val r = doProcessExitValue(command, input, output);
    if (r != 0) {
      throw new IOException("error: " + command.mkString(" "));
    }
  }

  def doProcessExitValue(command: List[String], input: Input, output: Output): Int = {
    val process1 = Process(command);
    val process2 = input match {
      case FileInput(path) =>
        process1 #< new File(path);
      case Stdin =>
        process1;
      case NullInput =>
        process1 #< nullInputStream;
    }
    val connectInput = input match {
      case FileInput(path) =>
        false;
      case Stdin =>
        true;
      case NullInput =>
        false;
    }
    val process3 = output match {
      case FileOutput(path) =>
        process2 #> new File(path);
      case Stdout =>
        process2;
      case NullOutput =>
        process2 #> dummyOutputStream;
    }
    val r = process3.run(connectInput).exitValue();
    r;
  }

  def mkfifo(path: String): Unit = {
    val command = List("mkfifo", path);
    doProcess(command, NullInput, NullOutput);
  }

  val executorContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool());

  private def nullInputStream: InputStream = {
    new ByteArrayInputStream(new Array[Byte](0));
  }

  private def dummyOutputStream: OutputStream = {
    OutputStream.nullOutputStream();
  }

  sealed trait Input;
  case class FileInput(path: String) extends Input;
  case object Stdin extends Input;
  case object NullInput extends Input;

  sealed trait Output;
  case class FileOutput(path: String) extends Output;
  case object Stdout extends Output;
  case object NullOutput extends Output;

}

