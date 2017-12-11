package eu.sim642.adventofcode2017

object Day11 {

  case class HexPos(x: Int, y: Int, z: Int) {
    require(x + y + z == 0)

    def +(that: HexPos): HexPos = HexPos(this.x + that.x, this.y + that.y, this.z + that.z)

    def manhattanDistance(that: HexPos): Int = ((this.x - that.x).abs + (this.y - that.y).abs + (this.z - that.z).abs) / 2

    def move(moves: Seq[String]): HexPos = moves.foldLeft(this)(_ + HexPos.neighbors(_))
  }

  object HexPos {
    val zero: HexPos = HexPos(0, 0, 0)

    val neighbors: Map[String, HexPos] = Map(
      "n" -> HexPos(0, 1, -1),
      "nw" -> HexPos(-1, 1, 0),
      "sw" -> HexPos(-1, 0, 1),
      "s" -> HexPos(0, -1, 1),
      "se" -> HexPos(1, -1, 0),
      "ne" -> HexPos(1, 0, -1)
    )
  }

  def fewestSteps(moves: Seq[String]): Int = HexPos.zero.move(moves) manhattanDistance HexPos.zero

  def fewestSteps(input: String): Int = fewestSteps(input.split(","))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(fewestSteps(input))
  }
}
