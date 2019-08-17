package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos4

case class Box4(min: Pos4, max: Pos4) extends BoxOps[Pos4, Box4] {
  override def factory: BoxFactory[Pos4, Box4] = Box4
}

object Box4 extends BoxFactory[Pos4, Box4] {

}
