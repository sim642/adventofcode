package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.pos.HexPos

object Day24 {

  private val neighbors: Map[String, HexPos] = Map(
    "e" -> HexPos(1, -1, 0),
    "se" -> HexPos(0, -1, 1),
    "sw" -> HexPos(-1, 0, 1),
    "w" -> HexPos(-1, 1, 0),
    "nw" -> HexPos(0, 1, -1),
    "ne" -> HexPos(1, 0, -1),
  )

  def applyMoves(pos: HexPos, moves: Seq[String]): HexPos = moves.foldLeft(pos)(_ + neighbors(_))

  def countBlackTiles(directions: Seq[Seq[String]]): Int = {
    directions
      .view
      .map(applyMoves(HexPos.zero, _))
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .filter(_._2 % 2 == 1)
      .keys
      .size
  }

  private val moveRegex = """e|se|sw|w|nw|ne""".r

  def parseMoves(s: String): Seq[String] = moveRegex.findAllIn(s).toSeq

  def parseDirections(input: String): Seq[Seq[String]] = input.linesIterator.map(parseMoves).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBlackTiles(parseDirections(input)))
  }
}
