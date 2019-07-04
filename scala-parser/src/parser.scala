// mulang-bin-sources: scala

object Main {
  def main(args: Array[String]): Unit = {
    val total = Parser.parseTotalCmdSeq(args.toList)
    println(total)
  }
}

case class TotalCmdSeq(globalOptions: List[GlobalOption], commandSeq: CommandSeq)

sealed trait GlobalOption {}
object GlobalOption {
  case class Tsv() extends GlobalOption
  case class Csv() extends GlobalOption
  case class Input(file: String) extends GlobalOption
  case class Output(file: String) extends GlobalOption
}

case class CommandSeq(input: Option[String], commands: List[Command])

sealed trait Command {}
object Command {
  case class Cut(option: List[Cut.Option]) extends Command

  object Cut {
    sealed trait Option {}
    object Option {
      case class Cols(cols: String) extends Option
      case class Help() extends Option
      case class G(g: GlobalOption) extends Option
    }
  }
}

object Parser {
  type OptResult[T] = (Option[T], List[String])
  type ListResult[T] = (List[T], List[String])

  def parseTotalCmdSeq(arg: List[String]): OptResult[TotalCmdSeq] = {
    val (options, rest) = parseMany(parseGlobalOption, arg)
    parseCommandSeq(rest) match {
      case (None, tail) => (None, arg)
      case (Some(cmdSeq), tail) => (Some(TotalCmdSeq(options, cmdSeq)), tail)
    }
  }

  def parseGlobalOption(arg: List[String]): OptResult[GlobalOption] = {
    arg match {
      case "--tsv" :: tail => (Some(GlobalOption.Tsv()), tail)
      case "--csv" :: tail => (Some(GlobalOption.Csv()), tail)
      case "-i" :: file :: tail if (exists(file)) => (Some(GlobalOption.Input(file)), tail)
      case "-o" :: file :: tail => (Some(GlobalOption.Output(file)), tail)
      case _ => (None, arg)
    }
  }

  def parseCommandSeq(arg: List[String]): OptResult[CommandSeq] = {
    val (input, rest) = parseInput(arg)
    val (cmds, rest2) = parseMany(parseCommand, rest)
    (Some(CommandSeq(input, cmds)), rest2)
  }

  def parseInput(arg: List[String]): OptResult[String] = {
    arg match {
      case file :: tail if exists(file) => (Some(file), tail)
      case _ => (None, arg)
    }
  }

  def parseCommand(arg: List[String]): OptResult[Command] = {
    arg match {
      case "cut" :: rest => {
        val (opts, rest2) = parseMany(parseCutOption, rest)
        (Some(Command.Cut(opts)), rest2)
      }
      case _ => (None, arg)
    }
  }

  def parseCutOption(arg: List[String]): OptResult[Command.Cut.Option] = {
    arg match {
      case "--col" :: cols :: tail => (Some(Command.Cut.Option.Cols(cols)), tail)
      case "--help" :: tail => (Some(Command.Cut.Option.Help()), tail)
      case _ => parseGlobalOption(arg) match {
        case (Some(g), tail) => (Some(Command.Cut.Option.G(g)), tail)
        case (None, cols :: tail) => (Some(Command.Cut.Option.Cols(cols)), tail)
        case _ => (None, arg)
      }
    }
  }

  def parseMany[T](f: List[String] => OptResult[T], arg: List[String]): ListResult[T] = {
    f(arg) match {
      case (None, tail) => (Nil, tail)
      case (Some(t), tail) => {
        val (x, y) = parseMany(f, tail)
        (t::x, y)
      }
    }
  }

  def exists(file: String): Boolean = {
    import java.nio.file.{Paths, Files}

    Files.exists(Paths.get(file))
  }
}
