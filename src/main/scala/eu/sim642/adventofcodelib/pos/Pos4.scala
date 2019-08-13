package eu.sim642.adventofcodelib.pos

case class Pos4(x: Int, y: Int, z: Int, w: Int) extends PosOps[Pos4] {
  override def +(that: Pos4): Pos4 =
    Pos4(x + that.x, y + that.y, z + that.z, w + that.w)

  override def *:(k: Int): Pos4 =
    Pos4(k * x, k * y, k * z, k * w)

  override def manhattanDistance(that: Pos4): Int =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs + (w - that.w).abs
}

object Pos4 extends PosFactory[Pos4] {
  override val zero: Pos4 = Pos4(0, 0, 0, 0)
}
