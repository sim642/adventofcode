package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos

case class Box(min: Pos, max: Pos) extends BoxOps[Pos, Box] {
  override def factory: BoxFactory[Pos, Box] = Box
}

object Box extends BoxFactory[Pos, Box] {

}
