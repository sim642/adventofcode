package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos4

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
}

object Box4 extends BoxFactory[Pos4, Box4] {

}
