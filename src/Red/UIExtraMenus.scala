package Red

import Red.Buttons.menuButton

import scala.collection.mutable.ArrayBuffer
import scala.swing.{Button, Component}

class UIExtraMenus(ui: UIInterface) {
  private val UI_DO = ui.UI_DO(_)

  val content: ArrayBuffer[Component] = new ArrayBuffer[Component]()

  def menus: Seq[Component] = content.toSeq

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

  content += pipeMenu
  if (Personalised.needsPandoc(ui.theSession.path))
    content += panDocButton

}
