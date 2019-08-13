package eu.sim642.adventofcodelib.pos

case class Pos3(x: Int, y: Int, z: Int) {
  def manhattanDistance(that: Pos3): Int =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs

  def +(that: Pos3): Pos3 =
    Pos3(x + that.x, y + that.y, z + that.z)
}
