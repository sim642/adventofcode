package eu.sim642.adventofcodelib

import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

object Geometry {

  /**
   * Calculates the area of a simple polygon using the shoelace formula.
   * @see [[https://en.wikipedia.org/wiki/Shoelace_formula]]
   */
  def polygonArea(poss: Seq[Pos]): Int = {
    ((poss.last +: poss).iterator
      .zipWithTail
      .map(_ cross _)
      .sum / 2).abs
  }
}
