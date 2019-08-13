package eu.sim642.adventofcodelib.pos

case class Pos3(x: Int, y: Int, z: Int) extends PosOps[Pos3] {
  override def +(that: Pos3): Pos3 =
    Pos3(x + that.x, y + that.y, z + that.z)

  override def *:(k: Int): Pos3 =
    Pos3(k * x, k * y, k * z)

  override def manhattanDistance(that: Pos3): Int =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
}

object Pos3 extends PosFactory[Pos3] {
  override val zero: Pos3 = Pos3(0, 0, 0)
}
