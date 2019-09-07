// mulang-bin-sources: scala

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.concurrent.Executors;
import scala.sys.process.Process;
import scala.concurrent.ExecutionContext;

object ProcessUtil {

  def doProcess(command: List[String]): Unit = {
    val process = Process(command) #< new ByteArrayInputStream(new Array[Byte](0));
    val r = process.run(false).exitValue();
    if (r != 0) {
      throw new IOException("error: " + command.mkString(" "));
    }
  }

  def mkfifo(path: String): Unit = {
    val command = List("mkfifo", path);
    doProcess(command);
  }

  def rm(path: String): Unit = {
    val command = List("rm", path);
    doProcess(command);
  }

  val executorContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool());

}

