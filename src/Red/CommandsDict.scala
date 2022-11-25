package Red

import EditSessionCommands._

object CommandsDict {

private val commands = List (
        "abbreviate" -> abbreviate, 
        "autoIndentNL" -> autoIndentNL, 
        "autoIndentSelection" -> autoIndentSelection, 
        "autoTab" -> autoTab, 
        "clearAll" -> clearAll, 
        "copy" -> copy, 
        "cut" -> cut, 
        "delete" -> delete, 
        "exchangeCut" -> exchangeCut, 
        "exchangeMark" -> exchangeMark, 
        "flip" -> flip, 
        "indentSelection" -> indentSelection,
        "indentOrTab" -> indentOrTab,
        "latexToPDF" -> latexToPDF, 
        "latexUnblock" -> latexUnblock, 
        "lowerCaseFilter" -> lowerCaseFilter, 
        "mouseUp" -> mouseUp, 
        "nextChar" -> nextChar, 
        "nextLine" -> nextLine, 
        "notifyNow" -> notifyNow, 
        "pandocToPDF" -> pandocToPDF, 
        "paste" -> paste, 
        "prevChar" -> prevChar, 
        "prevLine" -> prevLine, 
        "selectAll" -> selectAll, 
        "selectMatchingDown" -> selectMatchingDown, 
        "selectParagraph" -> selectParagraph,
        "styleAsItalic" -> styleAs("textit"),
        "styleAsBold" -> styleAs("textbf"),
        "styleAsSlant" -> styleAs("textsl"),
        "styleAsSans" -> styleAs("textsf"),
        "styleAsEmph" -> styleAs("emph"),
        "toEnd" -> toEnd,
        "toHome" -> toHome, 
        "undentSelection" -> undentSelection, 
        "unicode" -> unicode, 
        "upperCaseFilter" -> upperCaseFilter,
        )

  private val dict = new collection.mutable.HashMap[String, SessionCommand]

  locally {
    for { pair <- commands } dict.addOne(pair)
  }

  def apply(name: String): Option[SessionCommand] = dict.get(name)
}
