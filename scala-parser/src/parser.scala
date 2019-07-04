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
  case class Join(option: List[Join.Option]) extends Command
  case class InsConst(option: List[InsConst.Option]) extends Command

  object Cut {
    sealed trait Option {}
    object Option {
      case class Cols(cols: String) extends Option
      case class Help() extends Option
      case class G(g: GlobalOption) extends Option
    }
  }
  object Join {
    sealed trait Option {}
    object Option {
      case class File(file: String) extends Option
      case class SubSeq(subSeq: CommandSeq) extends Option
      case class InnerJoin() extends Option
      case class LeftOuterJoin() extends Option
      case class RightOuterJoin() extends Option
      case class FullOuterJoin() extends Option
      case class Help() extends Option
      case class G(g: GlobalOption) extends Option
    }
  }
  object InsConst {
    sealed trait Option {}
    object Option {
      case class Value(value: String) extends Option
      case class Dst(dst: String) extends Option
      case class ValueDst(value: String, dst: String) extends Option
      case class Help() extends Option
      case class G(g: GlobalOption) extends Option
    }
  }
}

object Parser {
  type OptResult[T] = (Option[T], List[String])
  type ListResult[T] = (List[T], List[String])

  implicit class ResultExt[T](val value: (T, List[String])) extends AnyVal {
    def map[U](f: T => U): (U, List[String]) = {
      val (x, y) = value
      (f(x), y)
    }
  }

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
      case "cut" :: rest => parseMany(parseCutOption, rest).map(opts => Some(Command.Cut(opts)))
      case "join" :: rest => parseMany(parseJoinOption, rest).map(opts => Some(Command.Join(opts)))
      case "insconst" :: rest => parseMany(parseJoinOption, rest).map(opts => Some(Command.Join(opts)))
      case _ => (None, arg)
    }
  }

  def parseCutOption(arg: List[String]): OptResult[Command.Cut.Option] = {
    arg match {
      case "--col" :: cols :: tail => (Some(Command.Cut.Option.Cols(cols)), tail)
      case "--help" :: tail => (Some(Command.Cut.Option.Help()), tail)
      case _ => parseGlobalOption(arg) match {
        case (Some(g), tail) => (Some(Command.Cut.Option.G(g)), tail)
        case (None, cols :: tail) if colname(cols) => (Some(Command.Cut.Option.Cols(cols)), tail)
        case _ => (None, arg)
      }
    }
  }

  def parseJoinOption(arg: List[String]): OptResult[Command.Join.Option] = {
    arg match {
      case file :: tail if exists(file) => (Some(Command.Join.Option.File(file)), tail)
      case "--file" :: "[" :: tail => {
        parseCommandSeq(tail) match {
          case (Some(seq), "]" :: tail2) => (Some(Command.Join.Option.SubSeq(seq)), tail2)
          case _ => (None, arg)
        }
      }
      case "--file" :: file :: tail => (Some(Command.Join.Option.File(file)), tail)
      case "[" :: tail => {
        parseCommandSeq(tail) match {
          case (Some(seq), "]" :: tail2) => (Some(Command.Join.Option.SubSeq(seq)), tail2)
          case _ => (None, arg)
        }
      }
      case "--inner" :: tail => (Some(Command.Join.Option.InnerJoin()), tail)
      case "--left-outer" :: tail => (Some(Command.Join.Option.LeftOuterJoin()), tail)
      case "--right-outer" :: tail => (Some(Command.Join.Option.RightOuterJoin()), tail)
      case "--full-outer" :: tail => (Some(Command.Join.Option.FullOuterJoin()), tail)
      case "--help" :: tail => (Some(Command.Join.Option.Help()), tail)
      case _ => parseGlobalOption(arg) match {
        case (Some(g), tail) => (Some(Command.Join.Option.G(g)), tail)
        case _ => (None, arg)
      }
    }
  }

  def parseInsConstOption(arg: List[String]): OptResult[Command.InsConst.Option] = {
    arg match {
      case "--value" :: value :: tail => (Some(Command.InsConst.Option.Value(value)), tail)
      case "--dst" :: value :: tail => (Some(Command.InsConst.Option.Dst(value)), tail)
      case "--help" :: tail => (Some(Command.InsConst.Option.Help()), tail)
      case _ => parseGlobalOption(arg) match {
        case (Some(g), tail) => (Some(Command.InsConst.Option.G(g)), tail)
        case (None, value :: dst :: tail) => (Some(Command.InsConst.Option.ValueDst(value, dst)), tail)
        case _ => (None, arg)
      }
    }
  }

  def parseMany[T](f: List[String] => OptResult[T], arg: List[String]): ListResult[T] = {
    f(arg) match {
      case (None, tail) => (Nil, tail)
      case (Some(t), tail) => parseMany(f, tail).map(x => t::x)
    }
  }

  def exists(file: String): Boolean = {
    import java.nio.file.{Paths, Files}

    Files.exists(Paths.get(file))
  }

  def colname(name: String): Boolean = {
    name match {
      case "]" | "cut" | "join" | "insconst" => false
      case _ => true
    }
  }
}
