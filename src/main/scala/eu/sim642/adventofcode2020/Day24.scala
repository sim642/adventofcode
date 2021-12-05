package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.pos.HexPos
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.IterableImplicits._

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

  def getBlackTiles(directions: Seq[Seq[String]]): Set[HexPos] = {
    directions
      .view
      .map(applyMoves(HexPos.zero, _))
      .groupCount(identity)
      .filter(_._2 % 2 == 1)
      .keySet
  }

  def countBlackTiles(directions: Seq[Seq[String]]): Int = getBlackTiles(directions).size

  def step(blackTiles: Set[HexPos]): Set[HexPos] = {
    // based on 2020 Day 17
    blackTiles.iterator
      .flatMap(pos =>
        neighbors.values.iterator
          .map(pos + _)
      )
      .groupCount(identity)
      .collect({
        case (pos, 2) => pos
        case (pos, 1) if blackTiles(pos) => pos
      })
      .toSet
  }

  def countBlackTilesAfter(directions: Seq[Seq[String]], days: Int = 100): Int = {
    val initialBlackTiles = getBlackTiles(directions)
    val finalBlackTiles = Iterator.iterate(initialBlackTiles)(step)(days)
    finalBlackTiles.size
  }


  private val moveRegex = """e|se|sw|w|nw|ne""".r

  def parseMoves(s: String): Seq[String] = moveRegex.findAllIn(s).toSeq

  def parseDirections(input: String): Seq[Seq[String]] = input.linesIterator.map(parseMoves).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBlackTiles(parseDirections(input)))
    println(countBlackTilesAfter(parseDirections(input)))
  }
}
