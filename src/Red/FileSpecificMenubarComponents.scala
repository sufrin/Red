package Red

/**
 *
 *  Defines the File-specific buttons and menus that will appear on the menu bar.
 *
 * @param ui the user interface from/into which these components will acquire functionality
 *
 */

class FileSpecificMenubarComponents(ui: UIInterface) {

  import Red.Buttons.menuButton
  import scala.collection.mutable.ArrayBuffer
  import scala.swing.FileChooser.Result.{Approve, Cancel}
  import scala.swing.{Button, Component, Label, Menu}

  private val UI_DO = ui.UI_DO(_)

  private val _components: ArrayBuffer[Component] = new ArrayBuffer[Component]()
  def +=(component: Component): Unit = _components += component

  def components: Seq[Component] = _components.toSeq

  def pipeMenu: Utils.Menu = new Utils.Menu("Pipe") {
      // should piped output replace the selection or prefix it
      var augmentSelection: Boolean = false

      // Pipe the selection through ...
      contents += menuButton("\u24b6", "Pipe the selection through the shell command \u24b6 (see also \"Append Selection\")") {
        ui.withFilterWarnings("\u24b6") {
          ui.UI_DO(EditSessionCommands.pipeThrough(ui.argLine.text, replaceSelection = !augmentSelection))
        }
      }

      for {program <- Personalised.pipeShellCommands(ui.theSession.path)} {
        contents += menuButton(s"$program", s"Pipe the selection through the shell command \"$program\" (see also \"(++sel'n)\"") {
          ui.withFilterWarnings(s"$program") {
            UI_DO(EditSessionCommands.pipeThrough(program, replaceSelection = !augmentSelection))
          }
        }
      }

      contents += Separator()

      for {program <- Personalised.pipeRedScripts(ui.theSession.path)} {
        contents += menuButton(s"$program", s"Evaluate the Redscript ($program <the session path> Ⓐ \u24bb \u24c7 <the selection>)  (see also \"(++sel'n)\"") {
          ui.withFilterWarnings(s"$program") {
            UI_DO(EditSessionCommands.pipeThroughScript(program, ui.theSession.path, ui.argLine.text, ui.findLine.text, ui.replLine.text, replaceSelection = !augmentSelection))
          }
        }
      }

      contents += Separator()

      contents += new Buttons.PersistentCheckItem("(++sel'n)", "appendselectiontopipedoutput", { b => augmentSelection = b }, augmentSelection) {
        tooltip = "When enabled, the original selection is appended to the piped output from the above commands."
        font = Utils.buttonFont
      }

    } // Pipe Menu


  def panDocButton: Button =
    Buttons.Button("Pandoc", toolTip = "Run redpandoc now") {
      ui.saveOperation()
      UI_DO(EditSessionCommands.pandocToPDF)
    }

  def addLatexMenus(): Unit = {
    import Menus.EmbeddedDynamicMenu

    this += new Label("     ")

    this += Buttons.SwingButton("Tex", toolTip = "Run redpdf now") {
      ui.saveOperation()
      UI_DO(EditSessionCommands.latexToPDF)
    }

    val headers = Red.Personalised.latexSnippets(ui.theSession.path)

    this += new EmbeddedDynamicMenu("\\begin{...}", {
      Red.Personalised.latexBlockTypes(ui.theSession.path)
    }) {
      font = Utils.menuButtonFont
      prefix += menuButton("%%%%%%%%") {
        val header =
          """%%%%%%%%%%%%%%%%%%%%%%%%
            |%%%%%%%%%%%%%%%%%%%%%%%%
            |%%%%%%%%%%%%%%%%%%%%%%%%
            |""".stripMargin
        UI_DO(EditSessionCommands.latexInsert(header))
      }

      prefix += Separator()

      def component(block: String): Component = {
        if (block == "-")
          Separator()
        else
          menuButton(s"""\\begin{$block}""") {
            UI_DO(EditSessionCommands.latexBlock(block))
          }
      }

      // contents ++= dynamic

      suffix += Separator()
      suffix += menuButton("\\begin{\u24b6}", "Embed selection in latex block named in \u24b6") {
        UI_DO(EditSessionCommands.latexBlock(ui.argLine.text.trim))
      }

      suffix += menuButton("""\begin{...}->...""", "Extract content of selected latex block") {
        UI_DO(EditSessionCommands.latexUnblock)
      }

      suffix += Separator()

      // Infrequent additions
      suffix += new Menu("Tex") {

        for {(button, header) <- headers}
          contents += menuButton(button) {
            UI_DO(EditSessionCommands.latexInsert(header))
          }
        if (headers.isEmpty) { // PRO-TEM until we get template-definition implemented in RedScript
          contents += menuButton("\\documentclass{article}") {
            val up = "\\usepackage[]{}"
            val header =
              s"""\\documentclass[11pt,a4paper]{article}
                 |%%%%%%%%%%%%%%%%%%%%%
                 |$up
                 |%%%%%%%%%%%%%%%%%%%%%
                 |\\author{}
                 |\\title{}
                 |\\date{}
                 |%%%%%%%%%%%%%%%%%%%%%
                 |\\begin{document}
                 |\\maketitle
                 |
                 |\\end{document}
                 |""".stripMargin
            UI_DO(EditSessionCommands.latexInsert(header))
          }
        }
        if (headers.isEmpty) { // PRO-TEM
          contents +=
            menuButton(
              "\\documentclass{letter}") {
              val header =
                s"""\\documentclass[12pt,lab|wor|home|magd,bernard|sufrin]{letter} %
                   |\\To{lines\\\\of\\\\mailing address}
                   |\\Dear[Dear]{Victim}
                   |\\Re{subject matter}
                   |    This is the body
                   |\\Ps{ps paragraph}
                   |\\PostSig{post signature para}
                   |\\Cc{carrbon1, carbon2, ...}
                   |\\Sign[yours sincerely]{Bernard Sufrin}
                   |""".stripMargin
              UI_DO(EditSessionCommands.latexInsert(header))
            }
        }
        contents += Separator()
        contents += Separator()

        contents += menuButton("Tex source := \u24b6", toolTip = "Change default tex source using dialogue or nonempty \u24b6 field") {
          var text = ui.argLine.text.trim
          if (text.isEmpty) {
            val chooser = ui.fileChooser
            chooser.showOpenDialog(ui.top) match {
              case Approve => text = chooser.selectedFile.toString
              case Cancel => text = ""
            }
          }
          if (text.nonEmpty) ui.theSession.TEX = Utils.toPath(text)
          ui.feedbackPersistently(s"Tex source: ${ui.theSession.TEX.toString}")
        }

        contents += menuButton(s"Default tex source := ${ui.theSession.path}", toolTip = "Change default latex source to current file") {
          ui.theSession.TEX = Utils.toPath(ui.theSession.path)
          ui.feedbackPersistently(s"Tex source: ${ui.theSession.TEX.toString}")
        }
      }

    }

  }

  this += pipeMenu

  if (Personalised.needsPandoc(ui.theSession.path))
    this += panDocButton

  if (Red.Personalised.needsLatex(ui.theSession.path))
    addLatexMenus()

}
