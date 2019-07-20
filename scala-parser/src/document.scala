// mulang-bin-sources: scala

object Document {

  import OptionParser.HelpDocument;
  import OptionParser.Completion;

  def helpGlobalOptions = new HelpDocument {

    def document: List[String] = List(
      "--tsv",
      "--csv",
    );

  }

  def completionGlobalOptions = new Completion {

    def isFilePath: Boolean = true;
    def options: List[String] = List(
      "--tsv",
      "--csv",
      "-i",
      "-o",
      "--help",
    );
    def commandsEnable: Boolean = true;

  }

  def completionInputFile = new Completion {
    def isFilePath: Boolean = true;
    def options: List[String] = Nil;
    def commandsEnable: Boolean = false;
  }

  def completionOutputFile = new Completion {
    def isFilePath: Boolean = true;
    def options: List[String] = Nil;
    def commandsEnable: Boolean = false;
  }

  def toBashCompletion(completion: Completion): List[String] = {
    "TODO" :: Nil;
  }

  def toZshCompletion(completion: Completion): List[String] = {
    (
      if (completion.isFilePath) {
        "_files ." :: Nil;
      } else {
        Nil;
      }
    ) ::: (
      if (!completion.options.isEmpty) {
        "local -a op" ::
        ("op=(" + completion.options.map { o =>
          "'" + o + "'"; // TODO escape
        }.mkString(" ") + ")") ::
        "_describe -t opt option op" ::
        Nil;
      } else {
        Nil;
      }
    ) ::: (
      if (completion.commandsEnable) {
        val commands: List[String] = OptionParser.commands.keys.toList.sorted;
        "local -a cmd" ::
        ("cmd=(" + commands.map { c =>
          "'" + c + "'"; // TODO escape
        }.mkString(" ") + ")") ::
        "_describe -t cmd command cmd" ::
        Nil;
      } else {
        Nil;
      }
    );
  }

}
