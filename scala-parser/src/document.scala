// mulang-bin-sources: scala

object Document {

  import OptionParser.HelpDocument;

  def helpGlobalOptions = new HelpDocument {

    def isFilePath: Boolean = true;
    def options: List[String] = List(
      "--tsv",
      "--csv",
      "-i",
      "--help",
    );
    def commandsEnable: Boolean = true;

    def document: String = {
      """|--tsv
         |--csv""".stripMargin;
    }

  }

  def helpInputFile = new HelpDocument {

    def isFilePath: Boolean = true;
    def options: List[String] = Nil;
    def commandsEnable: Boolean = false;

    def document: String = {
      """|-i <FILEPATH>""".stripMargin;
    }

  }

  def toBashCompletion(help: HelpDocument): List[String] = {
    "TODO" :: Nil;
  }

  def toZshCompletion(help: HelpDocument): List[String] = {
    (
      if (help.isFilePath) {
        "_files ." :: Nil;
      } else {
        Nil;
      }
    ) ::: (
      if (!help.options.isEmpty) {
        "local -a op" ::
        ("op=(" + help.options.map { o =>
          "'" + o + "'"; // TODO escape
        }.mkString(" ") + ")") ::
        "_describe -t opt options op" ::
        Nil;
      } else {
        Nil;
      }
    ) ::: (
      if (help.commandsEnable) {
        val commands: List[String] = OptionParser.commands.keys.toList.sorted;
        "local -a cmd" ::
        ("cmd=(" + commands.map { c =>
          "'" + c + "'"; // TODO escape
        }.mkString(" ") + ")") ::
        "_describe -t cmd commands cmd" ::
        Nil;
      } else {
        Nil;
      }
    );
  }

}
