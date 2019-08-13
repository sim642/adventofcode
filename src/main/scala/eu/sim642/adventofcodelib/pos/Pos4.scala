package eu.sim642.adventofcodelib.pos

case class Pos4(x: Int, y: Int, z: Int, w: Int) {
  def manhattanDistance(that: Pos4): Int =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs + (w - that.w).abs
}
