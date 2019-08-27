package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._

object Day6 {

  sealed trait Operation
  case object TurnOn extends Operation
  case object TurnOff extends Operation
  case object Toggle extends Operation

  case class Instruction(operation: Operation, box: Box)

  def countLit(instructions: Seq[Instruction]): Int = {
    // TODO: optimize, possibly using mutable
    val initialGrid = Vector.fill(1000, 1000)(false)
    val finalGrid = instructions.foldLeft(initialGrid)({ case (grid, instruction) =>
      instruction.box.iterator.foldLeft(grid)({ case (grid, pos) =>
        grid.updatedGrid(pos, instruction.operation match {
          case TurnOn => true
          case TurnOff => false
          case Toggle => !grid(pos)
        })
      })
    })
    finalGrid.countGrid(identity)
  }

  def countLit(input: String): Int = countLit(parseInstructions(input))


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

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countLit(input))
  }
}
