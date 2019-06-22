package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2017.Day3.Pos
import eu.sim642.adventofcode2018.Day10.PosScalar
import eu.sim642.adventofcode2018.Day13.DirectionPos

import scala.util.matching.Regex

object Day1 {

  sealed trait Instruction {
    val n: Int
  }
  case class Left(n: Int) extends Instruction
  case class Right(n: Int) extends Instruction

  case class State(pos: Pos, dir: Pos) {
    def apply(instruction: Instruction): State = {
      val newDir = instruction match {
        case Left(_) => dir.left
        case Right(_) => dir.right
      }
      State(pos + instruction.n *: newDir, newDir)
    }
  }

  object State {
    val initial: State = State(Pos(0, 0), Pos(0, 1))
  }

  def shortestDestinationDist(instructions: Seq[Instruction]): Int = {
    val finalState = instructions.foldLeft(State.initial)(_(_))
    finalState.pos manhattanDistance Pos(0, 0)
  }

  private val instructionRegex: Regex = """([LR])(\d+)""".r

  def parseInstruction(s: String): Instruction = s match {
    case instructionRegex(dir, n) =>
      dir match {
        case "L" => Left(n.toInt)
        case "R" => Right(n.toInt)
      }
  }

  def parseInstructions(input: String): Seq[Instruction] = {
    input.split(", ").map(parseInstruction)
  }

  def shortestDestinationDist(input: String): Int = shortestDestinationDist(parseInstructions(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(shortestDestinationDist(input))
  }
}
