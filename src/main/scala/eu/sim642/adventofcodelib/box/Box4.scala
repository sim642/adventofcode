package eu.sim642.adventofcodelib.box

import eu.sim642.adventofcodelib.pos.Pos4

case class Box4(min: Pos4, max: Pos4) {
  def intersect(that: Box4): Option[Box4] = {
    val Box4(min2, max2) = that
    val minX = min.x max min2.x
    val maxX = max.x min max2.x
    val minY = min.y max min2.y
    val maxY = max.y min max2.y
    val minZ = min.z max min2.z
    val maxZ = max.z min max2.z
    val minW = min.w max min2.w
    val maxW = max.w min max2.w
    if (minX <= maxX && minY <= maxY && minZ <= maxZ && minW <= maxW)
      Some(Box4(Pos4(minX, minY, minZ, minW), Pos4(maxX, maxY, maxZ, maxW)))
    else
      None
  }
}
