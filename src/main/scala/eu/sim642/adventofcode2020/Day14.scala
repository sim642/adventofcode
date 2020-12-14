package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.StringImplicits._
import eu.sim642.adventofcodelib.LazyListImplicits._

object Day14 {

  type Address = Int
  type Integer = Long

  sealed trait Instruction
  case class Mask(mask: String) extends Instruction
  case class Mem(address: Address, value: Integer) extends Instruction

  type Instructions = List[Instruction]

  type Memory = Map[Address, Integer]

  case class ProgramState(instructions: Instructions, memory: Memory = Map.empty.withDefaultValue(0L), mask0: Integer = Long.MinValue, mask1: Integer = 0L) {
    def execOne: Option[ProgramState] = instructions match {
      case Nil => None
      case instruction :: newInstructions =>
        Some(
          instruction match {
            case Mask(mask) =>
              val mask0 = mask.map({
                case '0' => '0'
                case _ => '1'
              }).toLongRadix(2)
              val mask1 = mask.map({
                case '1' => '1'
                case _ => '0'
              }).toLongRadix(2)
              copy(instructions = newInstructions, mask0 = mask0, mask1 = mask1)
            case Mem(address, value) =>
              val newValue = (value & mask0) | mask1
              copy(instructions = newInstructions, memory = memory + (address -> newValue))
          }
        )
    }

    def execs: LazyList[ProgramState] = LazyList.unfold0(this)(_.execOne)
  }

  def sumFinalMemory(instructions: Instructions): Integer = {
    ProgramState(instructions).execs
      .last
      .memory
      .values
      .sum
  }


  private val maskRegex = """mask = ([01X]{36})""".r
  private val memRegex = """mem\[(\d+)\] = (\d+)""".r

  def parseInstruction(s: String): Instruction = s match {
    case maskRegex(mask) => Mask(mask)
    case memRegex(address, value) => Mem(address.toInt, value.toLong)
  }

  def parseInstructions(input: String): Instructions = input.linesIterator.map(parseInstruction).toList

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumFinalMemory(parseInstructions(input)))
  }
}
