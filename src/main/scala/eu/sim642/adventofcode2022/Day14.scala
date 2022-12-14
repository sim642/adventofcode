package eu.sim642.adventofcode2022

import Day14.Tile.*
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits.*

import scala.annotation.tailrec

object Day14 {

  // Based on 2018 day 17

  enum Tile(override val toString: String) {
    case Air extends Tile(".")
    case Rock extends Tile("#")
    case Sand extends Tile("o")
  }

  type Tiles = Map[Pos, Tile]
  
  private val pourOffsets = List(
    Pos(0, 1),
    Pos(-1, 1),
    Pos(1, 1)
  )

  def pour(tiles: Tiles, maxY: Int, pos: Pos): Tiles = {
    //printTiles(tiles)
    //println()
    if (pos.y > maxY)
      tiles
    else {
      tiles(pos) match {
        case Sand | Rock => tiles
        case Air =>

          @tailrec
          def helper(tiles: Tiles, offsets: List[Pos]): Tiles = offsets match {
            case Nil => tiles + (pos -> Sand)
            case offset :: newOffsets =>
              val newPos = pos + offset
              val newTiles = pour(tiles, maxY, newPos)
              newTiles(newPos) match {
                case Air => newTiles
                case Sand | Rock => helper(newTiles, newOffsets)
              }
          }

          helper(tiles, pourOffsets)
      }
    }
  }

  trait Part {
    def countRestingSand(tiles: Tiles): Int
  }

  object Part1 extends Part {
    override def countRestingSand(tiles: Tiles): Int = {
      val Box(min, max) = Box.bounding(tiles.keys)
      val poured = pour(tiles, max.y, Pos(500, 0))
      //printTiles(poured)
      poured.count({ case (pos, tile) => tile == Sand })
    }
  }

  object Part2 extends Part {
    override def countRestingSand(tiles: Tiles): Int = {
      val Box(min, max) = Box.bounding(tiles.keys)
      val maxY = max.y + 2
      val pos1 = Pos(500 - maxY, maxY)
      val pos2 = Pos(500 + maxY, maxY)
      val newTiles = tiles ++ makeLine(pos1, pos2)
      val poured = pour(newTiles, maxY, Pos(500, 0))
      //printTiles(poured)
      poured.count({ case (pos, tile) => tile == Sand })
    }
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

  def parseTiles(input: String): Tiles =
    input.linesIterator.map(parsePath).reduce(_ ++ _).withDefaultValue(Air)

  def printTiles(tiles: Tiles): Unit = {
    val Box(min, max) = Box.bounding(tiles.keys)
    for (y <- min.y to max.y) {
      for (x <- min.x to max.x)
        print(tiles(Pos(x, y)))
      println()
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countRestingSand(parseTiles(input)))
    println(Part2.countRestingSand(parseTiles(input)))
  }
}
