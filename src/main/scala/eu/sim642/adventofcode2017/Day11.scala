package eu.sim642.adventofcode2017

object Day11 {

  /**
    * https://www.redblobgames.com/grids/hexagons/
    */
  case class HexPos(x: Int, y: Int, z: Int) {
    require(x + y + z == 0)

    def +(that: HexPos): HexPos = HexPos(this.x + that.x, this.y + that.y, this.z + that.z)

    def manhattanDistance(that: HexPos): Int = ((this.x - that.x).abs + (this.y - that.y).abs + (this.z - that.z).abs) / 2

    def move(move: String): HexPos = this + HexPos.neighbors(move)

    def move(moves: Seq[String]): HexPos = moves.foldLeft(this)(_.move(_))

    def moves(moves: Seq[String]): Seq[HexPos] = moves.scanLeft(this)(_.move(_))
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

  def fewestSteps(input: String): Int = fewestSteps(input.split(",").toSeq)


  def furthestSteps(moves: Seq[String]): Int = HexPos.zero.moves(moves).map(_ manhattanDistance HexPos.zero).max

  def furthestSteps(input: String): Int = furthestSteps(input.split(",").toSeq)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(fewestSteps(input))
    println(furthestSteps(input))
  }
}
