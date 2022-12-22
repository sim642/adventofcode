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

  // manually hard-coded wrap around for example
  def exampleWrap(posFacing: PosFacing): PosFacing = {
    val PosFacing(pos, facing) = posFacing
    facing match {
      case Pos(1, 0) => // right
        if (pos.x == 11 && (4 until 8).contains(pos.y)) {
          val i = pos.y - 4
          PosFacing(Pos(16 - 1 - i, 8), Pos(0, 1))
        }
        else
          ???
      case Pos(0, 1) => // down
        if (pos.y == 11 && (8 until 12).contains(pos.x)) {
          val i = pos.x - 8
          PosFacing(Pos(4 - 1 - i, 7), Pos(0, -1))
        }
        else
          ???
      case Pos(-1, 0) => // left
        ???
      case Pos(0, -1) => // up
        if (pos.y == 4 && (4 until 8).contains(pos.x)) {
          val i = pos.x - 4
          PosFacing(Pos(8, i), Pos(1, 0))
        }
        else
          ???
    }
  }

  // manually hard-coded wrap around for my input
  def inputWrap(posFacing: PosFacing): PosFacing = {
    val PosFacing(pos, facing) = posFacing
    facing match {
      case Pos(1, 0) => // right
        if (pos.x == 149 && (0 until 50).contains(pos.y)) {
          val i = pos.y
          PosFacing(Pos(99, 150 - 1 - i), Pos(-1, 0))
        }
        else if (pos.x == 99 && (100 until 150).contains(pos.y)) {
          val i = pos.y - 100
          PosFacing(Pos(149, 50 - 1 - i), Pos(-1, 0))
        }
        else if (pos.x == 49 && (150 until 200).contains(pos.y)) {
          val i = pos.y - 150
          PosFacing(Pos(50 + i, 149), Pos(0, -1))
        }
        else if (pos.x == 99 && (50 until 100).contains(pos.y)) {
          val i = pos.y - 50
          PosFacing(Pos(100 + i, 49), Pos(0, -1))
        }
        else
          ???
      case Pos(0, 1) => // down
        if (pos.y == 149 && (50 until 100).contains(pos.x)) {
          val i = pos.x - 50
          PosFacing(Pos(49, 150 + i), Pos(-1, 0))
        }
        else if (pos.y == 49 && (100 until 150).contains(pos.x)) {
          val i = pos.x - 100
          PosFacing(Pos(99, 50 + i), Pos(-1, 0))
        }
        else if (pos.y == 199 && (0 until 50).contains(pos.x)) {
          val i = pos.x
          PosFacing(Pos(100 + i, 0), Pos(0, 1))
        }
        else
          ???
      case Pos(-1, 0) => // left
        if (pos.x == 0 && (150 until 200).contains(pos.y)) {
          val i = pos.y - 150
          PosFacing(Pos(50 + i, 0), Pos(0, 1))
        }
        else if (pos.x == 50 && (0 until 50).contains(pos.y)) {
          val i = pos.y
          PosFacing(Pos(0, 150 - 1 - i), Pos(1, 0))
        }
        else if (pos.x == 0 && (100 until 150).contains(pos.y)) {
          val i = pos.y - 100
          PosFacing(Pos(50, 50 - 1 - i), Pos(1, 0))
        }
        else if (pos.x == 50 && (50 until 100).contains(pos.y)) {
          val i = pos.y - 50
          PosFacing(Pos(i, 100), Pos(0, 1))
        }
        else
          ???
      case Pos(0, -1) => // up
        if (pos.y == 0 && (50 until 100).contains(pos.x)) {
          val i = pos.x - 50
          PosFacing(Pos(0, 150 + i), Pos(1, 0))
        }
        else if (pos.y == 100 && (0 until 50).contains(pos.x)) {
          val i = pos.x
          PosFacing(Pos(50, 50 + i), Pos(1, 0))
        }
        else if (pos.y == 0 && (100 until 150).contains(pos.x)) {
          val i = pos.x - 100
          PosFacing(Pos(i, 199), Pos(0, -1))
        }
        else
          ???
    }
  }

  // TODO: deduplicate
  def followPath2(input: Input, wrap: PosFacing => PosFacing): PosFacing = {
    val Input(map, path) = input

    def moveForward(posFacing: PosFacing): PosFacing = {
      //println(s"  $posFacing")
      val PosFacing(pos, facing) = posFacing

      val newPos = pos + facing
      val newPosFacing =
        if (!map.containsPos(newPos) || map(newPos) == ' ') {
          val w = wrap(posFacing)
          //println(s"      $w")
          //assert(wrap(w.copy(facing = -w.facing)) == PosFacing(pos, -facing))
          w
        }
        else
          PosFacing(newPos, facing)

      //println(s"    $newPosFacing")

      map(newPosFacing.pos) match {
        case '.' => newPosFacing
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

    val initialX = map.head.indexOf('.')
    val initialPosFacing = PosFacing(Pos(initialX, 0), Pos(1, 0))

    path.foldLeft(initialPosFacing)(move)
  }

  def finalPassword2(input: Input, wrap: PosFacing => PosFacing): Int = {
    val finalPosFacing = followPath2(input, wrap)
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
    println(finalPassword2(parseInput(input), inputWrap))

    // part 2: 120330 - too low
  }
}
