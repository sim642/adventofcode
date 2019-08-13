package eu.sim642.adventofcodelib.pos

case class Pos(x: Int, y: Int) {
  def manhattanDistance(other: Pos): Int = (x - other.x).abs + (y - other.y).abs

  def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
}

object Pos {
  val axisOffsets: Seq[Pos] = Seq(Pos(0, 1), Pos(-1, 0), Pos(1, 0), Pos(0, -1))
  val diagonalOffsets: Seq[Pos] = Seq(Pos(-1, 1), Pos(1, 1), Pos(-1, -1), Pos(1, -1))
  val allOffsets: Seq[Pos] = axisOffsets ++ diagonalOffsets
}
