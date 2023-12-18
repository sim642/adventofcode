package eu.sim642.adventofcodelib

import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

import scala.math.Integral.Implicits.infixIntegralOps

object Geometry {

  /**
   * Calculates the area of a simple polygon using the shoelace formula.
   * @see [[https://en.wikipedia.org/wiki/Shoelace_formula]]
   */
  def polygonArea[A](poss: collection.Seq[Pos])(using aIntegral: Integral[A]): A = {
    ((poss.last +: poss).iterator
      .zipWithTail
      .map(_ cross _)
      .sum / aIntegral.fromInt(2)).abs
  }
}
