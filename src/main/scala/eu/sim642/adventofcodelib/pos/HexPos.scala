package eu.sim642.adventofcodelib.pos

/**
  * https://www.redblobgames.com/grids/hexagons/
  */
case class HexPos(x: Int, y: Int, z: Int) extends PosOps[HexPos] {
  require(x + y + z == 0)

  override def +(that: HexPos): HexPos =
    HexPos(this.x + that.x, this.y + that.y, this.z + that.z)

  override def *:(k: Int): HexPos =
    HexPos(k * x, k * y, k * z)

  override infix def manhattanDistance(that: HexPos): Int =
    ((this.x - that.x).abs + (this.y - that.y).abs + (this.z - that.z).abs) / 2
}

object HexPos extends PosFactory[HexPos] {
  override val zero: HexPos = HexPos(0, 0, 0)
}
