package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos

case class Box(min: Pos, max: Pos) {
  def contains(pos: Pos): Boolean = {
    min.x <= pos.x && pos.x <= max.x &&
      min.y <= pos.y && pos.y <= max.y
  }
}
