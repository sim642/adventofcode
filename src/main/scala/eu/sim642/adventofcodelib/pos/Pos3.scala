package eu.sim642.adventofcodelib.pos

case class Pos3(x: Int, y: Int, z: Int) extends BoxPosOps[Pos3] {
  override def +(that: Pos3): Pos3 =
    Pos3(x + that.x, y + that.y, z + that.z)

  override def *:(k: Int): Pos3 =
    Pos3(k * x, k * y, k * z)

  override infix def manhattanDistance(that: Pos3): Int =
    (x - that.x).abs + (y - that.y).abs + (z - that.z).abs

  override def <=(that: Pos3): Boolean =
    x <= that.x && y <= that.y && z <= that.z

  override infix def min(that: Pos3): Pos3 =
    Pos3(x min that.x, y min that.y, z min that.z)

  override infix def max(that: Pos3): Pos3 =
    Pos3(x max that.x, y max that.y, z max that.z)
}

object Pos3 extends PosFactory[Pos3] {
  override val zero: Pos3 = Pos3(0, 0, 0)

  val axisOffsets: Seq[Pos3] = Seq(
    Pos3(1, 0, 0),
    Pos3(-1, 0, 0),
    Pos3(0, 1, 0),
    Pos3(0, -1, 0),
    Pos3(0, 0, 1),
    Pos3(0, 0, -1),
  )

  val allOffsets: Seq[Pos3] =
    for {
      z <- -1 to 1
      y <- -1 to 1
      x <- -1 to 1
      pos = Pos3(x, y, z)
      if pos != Pos3.zero
    } yield pos
}
