// mulang-bin-sources: scala

trait Completion {
  def isFilePath: Boolean;
  def parameters: List[String];
  def options: List[String];
  def commandsEnable: Boolean;
}

object Completion {

  def toZshCompletion(completion: Completion): List[String] = {
    (
      if (completion.isFilePath) {
        "_files" :: Nil;
      } else {
        Nil;
      }
    ) ::: (
      if (!completion.parameters.isEmpty) {
        "local -a param" ::
        ("param=(" + completion.options.map { a =>
          "'" + a + "'"; // TODO escape
        }.mkString(" ") + ")") ::
        "_describe -t param parameter param" ::
        Nil;
      } else {
        Nil;
      }
    ) ::: (
      if (!completion.options.isEmpty) {
        "local -a op" ::
        ("op=(" + completion.options.map { o =>
          "'" + o + "'"; // TODO escape
        }.mkString(" ") + ")") ::
        "_describe -t op option op" ::
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

