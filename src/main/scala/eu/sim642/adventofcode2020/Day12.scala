package eu.sim642.adventofcode2020

import Day12.Move._
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2018.Day13.DirectionPos

object Day12 {

  enum Move {
    case North(amount: Int)
    case South(amount: Int)
    case East(amount: Int)
    case West(amount: Int)
    case Left(degrees: Int)
    case Right(degrees: Int)
    case Forward(amount: Int)
  }

  case class Ship(pos: Pos, direction: Pos)

  sealed trait Part {
    protected def applyMove(ship: Ship, move: Move): Ship

    protected val initialDirection: Pos

    def movesDistance(moves: Seq[Move]): Int = {
      val initialShip = Ship(Pos.zero, initialDirection)
      val finalShip = moves.foldLeft(initialShip)(applyMove)
      initialShip.pos manhattanDistance finalShip.pos
    }
  }

  object Part1 extends Part {

    override def applyMove(ship: Ship, move: Move): Ship = {
      val Ship(pos, direction) = ship
      move match {
        case North(amount) => ship.copy(pos = pos + amount *: Pos(0, -1))
        case South(amount) => ship.copy(pos = pos + amount *: Pos(0, 1))
        case East(amount) => ship.copy(pos = pos + amount *: Pos(1, 0))
        case West(amount) => ship.copy(pos = pos + amount *: Pos(-1, 0))
        case Left(90) | Right(270) => ship.copy(direction = direction.left)
        case Left(180) | Right(180) => ship.copy(direction = -1 *: direction)
        case Left(270) | Right(90) => ship.copy(direction = direction.right)
        case Left(_) | Right(_) => throw new IllegalArgumentException("illegal degrees")
        case Forward(amount) => ship.copy(pos = pos + amount *: direction)
      }
    }

    override protected val initialDirection: Pos = Pos(1, 0)
  }

  object Part2 extends Part {

    override def applyMove(ship: Ship, move: Move): Ship = {
      val Ship(pos, direction) = ship
      move match {
        case North(amount) => ship.copy(direction = direction + amount *: Pos(0, -1))
        case South(amount) => ship.copy(direction = direction + amount *: Pos(0, 1))
        case East(amount) => ship.copy(direction = direction + amount *: Pos(1, 0))
        case West(amount) => ship.copy(direction = direction + amount *: Pos(-1, 0))
        case Left(90) | Right(270) => ship.copy(direction = direction.left)
        case Left(180) | Right(180) => ship.copy(direction = -1 *: direction)
        case Left(270) | Right(90) => ship.copy(direction = direction.right)
        case Left(_) | Right(_) => throw new IllegalArgumentException("illegal degrees")
        case Forward(amount) => ship.copy(pos = pos + amount *: direction)
      }
    }

    override protected val initialDirection: Pos = Pos(10, -1)
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

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.movesDistance(parseMoves(input)))
    println(Part2.movesDistance(parseMoves(input)))
  }
}
