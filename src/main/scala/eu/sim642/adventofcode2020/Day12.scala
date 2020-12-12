package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2018.Day13.DirectionPos

object Day12 {

  sealed trait Move
  case class North(amount: Int) extends Move
  case class South(amount: Int) extends Move
  case class East(amount: Int) extends Move
  case class West(amount: Int) extends Move
  case class Left(degrees: Int) extends Move
  case class Right(degrees: Int) extends Move
  case class Forward(amount: Int) extends Move

  sealed trait Part {
    def movesDistance(moves: Seq[Move]): Int
  }

  object Part1 extends Part {

    case class Ship(pos: Pos, direction: Pos) {
      def applyMove(move: Move): Ship = move match {
        case North(amount) => copy(pos = pos + amount *: Pos(0, -1))
        case South(amount) => copy(pos = pos + amount *: Pos(0, 1))
        case East(amount) => copy(pos = pos + amount *: Pos(1, 0))
        case West(amount) => copy(pos = pos + amount *: Pos(-1, 0))
        case Left(90) | Right(270) => copy(direction = direction.left)
        case Left(180) | Right(180) => copy(direction = -1 *: direction)
        case Left(270) | Right(90) => copy(direction = direction.right)
        case Forward(amount) => copy(pos = pos + amount *: direction)
      }
    }

    def movesDistance(moves: Seq[Move]): Int = {
      val initialShip = Ship(Pos.zero, Pos(1, 0))
      val finalShip = moves.foldLeft(initialShip)(_.applyMove(_))
      initialShip.pos manhattanDistance finalShip.pos
    }
  }

  object Part2 extends Part {

    case class Ship(pos: Pos, direction: Pos) {
      def applyMove(move: Move): Ship = move match {
        case North(amount) => copy(direction = direction + amount *: Pos(0, -1))
        case South(amount) => copy(direction = direction + amount *: Pos(0, 1))
        case East(amount) => copy(direction = direction + amount *: Pos(1, 0))
        case West(amount) => copy(direction = direction + amount *: Pos(-1, 0))
        case Left(90) | Right(270) => copy(direction = direction.left)
        case Left(180) | Right(180) => copy(direction = -1 *: direction)
        case Left(270) | Right(90) => copy(direction = direction.right)
        case Forward(amount) => copy(pos = pos + amount *: direction)
      }
    }

    def movesDistance(moves: Seq[Move]): Int = {
      val initialShip = Ship(Pos.zero, Pos(10, -1))
      val finalShip = moves.foldLeft(initialShip)(_.applyMove(_))
      initialShip.pos manhattanDistance finalShip.pos
    }
  }


  private val moveRegex = """([NSEWLRF])(\d+)""".r

  def parseMove(s: String): Move = s match {
    case moveRegex(action, valueStr) =>
      val value = valueStr.toInt
      action.head match {
        case 'N' => North(value)
        case 'S' => South(value)
        case 'E' => East(value)
        case 'W' => West(value)
        case 'L' => Left(value)
        case 'R' => Right(value)
        case 'F' => Forward(value)
      }
  }

  def parseMoves(input: String): Seq[Move] = input.linesIterator.map(parseMove).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.movesDistance(parseMoves(input)))
    println(Part2.movesDistance(parseMoves(input)))
  }
}
