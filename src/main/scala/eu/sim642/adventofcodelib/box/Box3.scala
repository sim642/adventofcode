package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos3

case class Box3(min: Pos3, max: Pos3) extends BoxOps[Pos3, Box3] {
  override def factory: BoxFactory[Pos3, Box3] = Box3

  override def iterator: Iterator[Pos3] = {
    for {
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
      z <- (min.z to max.z).iterator
    } yield Pos3(x, y, z)
  }

  /*def closestTo(pos: Pos3): Pos3 = {
    Pos3(
      clamp(min.x, max.x)(pos.x),
      clamp(min.y, max.y)(pos.y),
      clamp(min.z, max.z)(pos.z),
    )
  }*/
}

object Box3 extends BoxFactory[Pos3, Box3] {

}
