package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos

case class Box(min: Pos, max: Pos) extends BoxOps[Pos, Box] {
  override def factory: BoxFactory[Pos, Box] = Box

  override def iterator: Iterator[Pos] = {
    for {
      x <- (min.x to max.x).iterator
      y <- (min.y to max.y).iterator
    } yield Pos(x, y)
  }
}

object Box extends BoxFactory[Pos, Box] {

}
