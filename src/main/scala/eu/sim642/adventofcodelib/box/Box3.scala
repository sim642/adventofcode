package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos3

case class Box3(min: Pos3, max: Pos3) {
  def contains(pos: Pos3): Boolean = {
    min.x <= pos.x && pos.x <= max.x &&
      min.y <= pos.y && pos.y <= max.y &&
      min.z <= pos.z && pos.z <= max.z
  }

  /*def contains(octahedron: Nanobot): Boolean = octahedron.corners.forall(contains)

  def closestTo(pos: Pos3): Pos3 = {
    Pos3(
      clamp(min.x, max.x)(pos.x),
      clamp(min.y, max.y)(pos.y),
      clamp(min.z, max.z)(pos.z),
    )
  }*/
}
