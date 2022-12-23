package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos4

import scala.math.Numeric.Implicits.infixNumericOps

case class Box4(min: Pos4, max: Pos4) extends BoxOps[Pos4, Box4] {
  override def factory: BoxFactory[Pos4, Box4] = Box4

  override def iterator: Iterator[Pos4] = {
    for {
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
      z <- (min.z to max.z).iterator
      w <- (min.w to max.w).iterator
    } yield Pos4(x, y, z, w)
  }

  override def size[C](using cNumeric: Numeric[C]): C = {
    val d = max - min + Pos4(1, 1, 1, 1)
    cNumeric.fromInt(d.x) * cNumeric.fromInt(d.y) * cNumeric.fromInt(d.z) * cNumeric.fromInt(d.w)
  }
}

object Box4 extends BoxFactory[Pos4, Box4] {

}
