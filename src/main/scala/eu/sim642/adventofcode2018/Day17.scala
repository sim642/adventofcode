package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day3.Pos

object Day17 {

  sealed trait Tile
  case object Sand extends Tile {
    override def toString: String = "."
  }
  case object Clay extends Tile {
    override def toString: String = "#"
  }
  trait WaterTile extends Tile
  case object Flowing extends WaterTile {
    override def toString: String = "|"
  }
  case object Settling extends WaterTile {
    override def toString: String = "/"
  }
  case object Settled extends WaterTile {
    override def toString: String = "~"
  }

  type Tiles = Map[Pos, Tile]


  def settle(tiles: Tiles, pos: Pos): Tiles = {
    tiles(pos) match {
      case Settling =>
        settle(settle(tiles + (pos -> Settled), pos + Pos(-1, 0)), pos + Pos(1, 0))
      case _ => tiles
    }
  }


  def flood(tiles: Tiles, maxY: Int, pos: Pos, prevPos: Pos): Tiles = {
    //printTiles(tiles)
    //println()
    if (pos.y > maxY)
      tiles
    else {
      tiles(pos) match {
        case Clay | Flowing | Settled | Settling => tiles
        case Sand =>
          val downPos = pos + Pos(0, 1)
          val downTiles = flood(tiles + (pos -> Flowing), maxY, downPos, pos)
          downTiles(downPos) match {
            case Flowing | Settling => downTiles
            case Clay | Settled =>
              val leftPos = pos + Pos(-1, 0)
              val leftTiles = flood(downTiles, maxY, leftPos, pos)
              val rightPos = pos + Pos(1, 0)
              val rightTiles = flood(leftTiles, maxY, rightPos, pos)
              (rightTiles(leftPos), rightTiles(rightPos)) match {
                case (Clay | Settled | Settling, _) | (_, Clay | Settled | Settling) if prevPos == leftPos || prevPos == rightPos =>
                  rightTiles + (pos -> Settling)
                case (Clay | Settled | Settling, Clay | Settled | Settling) =>
                  settle(settle(rightTiles, leftPos), rightPos) + (pos -> Settled)
                case _ => rightTiles
              }
            case Sand => downTiles
          }
      }
    }
  }

  def floodedTiles(tiles: Tiles): Int = {
    val (min, max) = Day6.boundingRect(tiles.keys.toSeq)
    val flooded = flood(tiles, max.y, Pos(500, 0), Pos(500, -1))
    //printTiles(flooded)
    flooded.count({ case (pos, tile) => pos.y >= min.y && tile.isInstanceOf[WaterTile]})
  }

  def settledTiles(tiles: Tiles): Int = {
    val (min, max) = Day6.boundingRect(tiles.keys.toSeq)
    val flooded = flood(tiles, max.y, Pos(500, 0), Pos(500, -1))
    //printTiles(flooded)
    flooded.count({ case (pos, tile) => pos.y >= min.y && tile == Settled})
  }


  private val xyRegex = """x=(\d+), y=(\d+)..(\d+)""".r
  private val yxRegex = """y=(\d+), x=(\d+)..(\d+)""".r

  def parseLine(line: String): Tiles = line match {
    case xyRegex(x, y1, y2) => (y1.toInt to y2.toInt).map(Pos(x.toInt, _) -> Clay).toMap
    case yxRegex(y, x1, x2) => (x1.toInt to x2.toInt).map(Pos(_, y.toInt) -> Clay).toMap
  }

  def parseInput(input: String): Tiles = {
    input.linesIterator.map(parseLine).reduce(_ ++ _).withDefaultValue(Sand)
  }

  def printTiles(tiles: Tiles): Unit = {
    val (min, max) = Day6.boundingRect(tiles.keys.toSeq)
    for (y <- min.y to max.y) {
      for (x <- min.x to max.x)
        print(tiles(Pos(x, y)))
      println()
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(floodedTiles(parseInput(input)))
    println(settledTiles(parseInput(input)))

    // 40454 - too high
  }
}
