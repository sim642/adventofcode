package eu.sim642.adventofcode2022

import Day14.Tile.*
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits.*

import scala.annotation.tailrec

object Day14 {

  // Based on 2018 day 17

  enum Tile(override val toString: String) {
    case Rock extends Tile("#")
    case Sand extends Tile("o")
  }

  type Tiles = Map[Pos, Tile]
  
  private val pourOffsets = Seq(
    Pos(0, 1),
    Pos(-1, 1),
    Pos(1, 1)
  )

  def pour(tiles: Tiles, maxY: Int, sourcePos: Pos): Tiles = {

    @tailrec
    def pourOne(tiles: Tiles, pos: Pos): Tiles = {
      val newPos = pourOffsets.map(pos + _).find(!tiles.contains(_))
      newPos match {
        case None => pourOne(tiles + (pos -> Sand), sourcePos)
        case Some(newPos) if newPos.y > maxY => tiles
        case Some(newPos) => pourOne(tiles, newPos)
      }
    }

    pourOne(tiles, sourcePos)
  }

  def countRestingSand(tiles: Tiles): Int = {
    val Box(min, max) = Box.bounding(tiles.keys)
    val poured = pour(tiles, max.y, Pos(500, 0))
    //printTiles(poured)
    poured.count({ case (pos, tile) => tile == Sand })
  }


  def parsePos(s: String): Pos = s match {
    case s"$x,$y" => Pos(x.toInt, y.toInt)
  }

  def makeLine(pos1: Pos, pos2: Pos): Tiles = {
    if (pos1.y == pos2.y) // horizontal
      ((pos1.x min pos2.x) to (pos1.x max pos2.x)).map(Pos(_, pos1.y) -> Rock).toMap
    else if (pos1.x == pos2.x) // vertical
      ((pos1.y min pos2.y) to (pos1.y max pos2.y)).map(Pos(pos1.x, _) -> Rock).toMap
    else
      throw new IllegalArgumentException("invalid path segment")
  }

  def parsePath(s: String): Tiles = {
    s.split( " -> ")
      .iterator
      .map(parsePos)
      .zipWithTail
      .map(makeLine)
      .reduce(_ ++ _)
  }

  def parseTiles(input: String): Tiles = input.linesIterator.map(parsePath).reduce(_ ++ _)

  def printTiles(tiles: Tiles): Unit = {
    val Box(min, max) = Box.bounding(tiles.keys)
    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        val pos = Pos(x, y)
        print(if (tiles.contains(pos)) tiles(pos).toString else ".")
      }
      println()
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countRestingSand(parseTiles(input)))
  }
}
