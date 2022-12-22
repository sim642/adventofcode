package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.GridImplicits._

import java.util.StringTokenizer
import scala.jdk.CollectionConverters.*


object Day22 {

  enum Move {
    case Forward(n: Int)
    case Left
    case Right
  }

  case class Input(map: Grid[Char], path: Seq[Move])

  case class PosFacing(pos: Pos, facing: Pos)

  def followPath(input: Input): PosFacing = {
    val Input(map, path) = input
    val mapTranspose = map.transpose

    def moveForward(posFacing: PosFacing): PosFacing = {
      val PosFacing(pos, facing) = posFacing

      val newPos = pos + facing
      val newPos2 =
        if (!map.containsPos(newPos) || map(newPos) == ' ') {
          facing match {
            case Pos(1, 0) => Pos(map(pos.y).indexWhere(_ != ' '), pos.y)
            case Pos(0, 1) => Pos(pos.x, mapTranspose(pos.x).indexWhere(_ != ' '))
            case Pos(-1, 0) => Pos(map(pos.y).lastIndexWhere(_ != ' '), pos.y)
            case Pos(0, -1) => Pos(pos.x, mapTranspose(pos.x).lastIndexWhere(_ != ' '))
          }
        }
        else
          newPos

      map(newPos2) match {
        case '.' => PosFacing(newPos2, facing)
        case '#' => posFacing
      }
    }

    def move(posFacing: PosFacing, move: Move): PosFacing = {
      //println((posFacing, move))
      move match {
        case Move.Forward(n) => (0 until n).foldLeft(posFacing)((x, _) => moveForward(x))
        case Move.Left => posFacing.copy(facing = posFacing.facing.left)
        case Move.Right => posFacing.copy(facing = posFacing.facing.right)
      }
    }

    //println(map.head)
    val initialX = map.head.indexOf('.')
    val initialPosFacing = PosFacing(Pos(initialX, 0), Pos(1, 0))

    path.foldLeft(initialPosFacing)(move)
  }

  def finalPassword(input: Input): Int = {
    val finalPosFacing = followPath(input)
    //println(finalPosFacing)
    1000 * (finalPosFacing.pos.y + 1) + 4 * (finalPosFacing.pos.x + 1) + (finalPosFacing.facing match {
      case Pos(1, 0) => 0
      case Pos(0, 1) => 1
      case Pos(-1, 0) => 2
      case Pos(0, -1) => 3
    })
  }

  def parsePath(s: String): Seq[Move] = {
    val tok = new StringTokenizer(s, "LR", true)
    tok.asIterator().asScala.map({
      case "L" => Move.Left
      case "R" => Move.Right
      case s: String => Move.Forward(s.toInt)
    }).toSeq
  }

  def parseInput(input: String): Input = {
    val Array(mapStr, pathStr) = input.split("\n\n", 2)
    val mapLines = mapStr.linesIterator.toSeq
    // pad lines just for transpose in wrap around
    val maxMapLineLength = mapLines.view.map(_.length).max
    val paddedMapLines = mapLines.map(_.padTo(maxMapLineLength, ' '))
    val map = paddedMapLines.map(_.toVector).toVector
    val path = parsePath(pathStr)
    Input(map, path)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(finalPassword(parseInput(input)))
  }
}
