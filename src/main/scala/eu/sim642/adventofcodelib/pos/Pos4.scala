package eu.sim642.adventofcodelib.pos

case class Pos4(x: Int, y: Int, z: Int, w: Int) extends BoxPosOps[Pos4] {
  override def +(that: Pos4): Pos4 =
    Pos4(x + that.x, y + that.y, z + that.z, w + that.w)

  override def *:(k: Int): Pos4 =
    Pos4(k * x, k * y, k * z, k * w)

  override def manhattanDistance(that: Pos4): Int =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs + (w - that.w).abs

  override def <=(that: Pos4): Boolean =
    x <= that.x && y <= that.y && z <= that.z && w <= that.w

  override def min(that: Pos4): Pos4 =
    Pos4(x min that.x, y min that.y, z min that.z, w min that.w)

  override def max(that: Pos4): Pos4 =
    Pos4(x max that.x, y max that.y, z max that.z, w max that.w)
}

object Pos4 extends PosFactory[Pos4] {
  override val zero: Pos4 = Pos4(0, 0, 0, 0)
}
