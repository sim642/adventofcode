package eu.sim642.adventofcode2015

import Day6.Operation._
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._

import scala.collection.mutable

object Day6 {

  enum Operation {
    case TurnOn, TurnOff, Toggle
  }

  case class Instruction(operation: Operation, box: Box)

  def countLit(instructions: Seq[Instruction]): Int = {
    // TODO: abstract GridImplicits to work on mutable
    val grid = mutable.ArrayBuffer.fill(1000, 1000)(false)
    for (instruction <- instructions) {
      for (pos <- instruction.box.iterator) {
        grid(pos.y)(pos.x) = instruction.operation match {
          case TurnOn => true
          case TurnOff => false
          case Toggle => !grid(pos.y)(pos.x)
        }
      }
    }
    grid.iterator.map(_.count(identity)).sum
  }

  def countLit(input: String): Int = countLit(parseInstructions(input))

  def totalBrightness(instructions: Seq[Instruction]): Int = {
    // TODO: abstract GridImplicits to work on mutable
    val grid = mutable.ArrayBuffer.fill(1000, 1000)(0)
    for (instruction <- instructions) {
      for (pos <- instruction.box.iterator) {
        grid(pos.y)(pos.x) = instruction.operation match {
          case TurnOn => grid(pos.y)(pos.x) + 1
          case TurnOff => (grid(pos.y)(pos.x) - 1) max 0
          case Toggle => grid(pos.y)(pos.x) + 2
        }
      }
    }
    grid.iterator.map(_.sum).sum
  }

  def totalBrightness(input: String): Int = totalBrightness(parseInstructions(input))


  private val instructionRegex = """(turn (?:on|off)|toggle) (\d+),(\d+) through (\d+),(\d+)""".r

  def parseInstruction(s: String): Instruction = s match {
    case instructionRegex(operation, minX, minY, maxX, maxY) =>
      Instruction(
        operation match {
          case "turn on" => TurnOn
          case "turn off" => TurnOff
          case "toggle" => Toggle
        },
        Box(Pos(minX.toInt, minY.toInt), Pos(maxX.toInt, maxY.toInt))
      )
  }

  def parseInstructions(input: String): Seq[Instruction] = input.linesIterator.map(parseInstruction).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countLit(input))
    println(totalBrightness(input))
  }
}
