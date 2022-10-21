package Red

/**
 *  Glue provision: this uses non-lightweight `BoxPanel`s in its implementation
 *  as a matter of convenience. In principle
 */

import scala.swing.{BoxPanel, Component, Dimension, Orientation}

class HGlue extends BoxPanel(Orientation.Horizontal) {
  preferredSize = new Dimension(0, 0)
  minimumSize   = new Dimension(0, 0)
  maximumSize   = new Dimension(Int.MaxValue, 0)
}

class VGlue extends BoxPanel(Orientation.Vertical) {
  preferredSize = new Dimension(0, 0)
  minimumSize   = new Dimension(0, 0)
  maximumSize   = new Dimension(0, Int.MaxValue)
}

object Glue {
  def horizontal(): Component = new HGlue()
  def vertical():   Component = new VGlue()
  def quad(pix: Int, plus: Int=0, minus: Int=0): Component = new BoxPanel(Orientation.Horizontal)  {
    preferredSize = new Dimension(pix, 0)
    minimumSize   = new Dimension(pix-minus, 0)
    maximumSize   = new Dimension(pix+plus, 0)
 }
}
