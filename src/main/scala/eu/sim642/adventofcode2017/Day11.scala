package eu.sim642.adventofcode2017

import eu.sim642.adventofcodelib.pos.HexPos

object Day11 {

  private val neighbors: Map[String, HexPos] = Map(
    "n" -> HexPos(0, 1, -1),
    "nw" -> HexPos(-1, 1, 0),
    "sw" -> HexPos(-1, 0, 1),
    "s" -> HexPos(0, -1, 1),
    "se" -> HexPos(1, -1, 0),
    "ne" -> HexPos(1, 0, -1)
  )

  implicit class MoveHexPos(pos: HexPos) {
    def move(move: String): HexPos = pos + neighbors(move)

    def move(moves: Seq[String]): HexPos = moves.foldLeft(pos)(_.move(_))

    def moves(moves: Seq[String]): Seq[HexPos] = moves.scanLeft(pos)(_.move(_))
  }

  def fewestSteps(moves: Seq[String]): Int = HexPos.zero.move(moves) manhattanDistance HexPos.zero

  def fewestSteps(input: String): Int = fewestSteps(input.split(",").toSeq)


  def furthestSteps(moves: Seq[String]): Int = HexPos.zero.moves(moves).map(_ manhattanDistance HexPos.zero).max

  def furthestSteps(input: String): Int = furthestSteps(input.split(",").toSeq)

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(fewestSteps(input))
    println(furthestSteps(input))
  }
}
