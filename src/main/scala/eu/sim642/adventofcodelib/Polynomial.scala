package eu.sim642.adventofcodelib

import eu.sim642.adventofcodelib.pos.Pos

object Polynomial {

  def fitQuadratic(p1: Pos, p2: Pos, p3: Pos): Int => Long = {
    x =>
      p1.y.toLong * (x - p2.x) * (x - p3.x) / (p1.x - p2.x) / (p1.x - p3.x) +
        p2.y.toLong * (x - p1.x) * (x - p3.x) / (p2.x - p1.x) / (p2.x - p3.x) +
        p3.y.toLong * (x - p1.x) * (x - p2.x) / (p3.x - p1.x) / (p3.x - p2.x)
  }
}
