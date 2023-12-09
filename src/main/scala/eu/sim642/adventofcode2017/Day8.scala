package eu.sim642.adventofcode2017

import Day8.Operation._
import Day8.Comparison._
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day8 {

  type Register = String

  enum Operation {
    case Inc(amount: Int)
    case Dec(amount: Int)
  }

  enum Comparison {
    case Eq, NotEq, Less, LessEq, Greater, GreaterEq
  }

  case class Condition(register: Register, comparison: Comparison, amount: Int)

  case class Instruction(register: Register, operation: Operation, condition: Condition)

  private val instructionRegex = """(\w+) (inc|dec) (-?\d+) if (\w+) (==|!=|<|<=|>|>=) (-?\d+)""".r

  def parseInstruction(instructionStr: String): Instruction = instructionStr match {
    case instructionRegex(register, op, amount, conditionRegister, comp, conditionAmount) =>
      val operation = (op match {
        case "inc" => Inc.apply _
        case "dec" => Dec.apply _
      })(amount.toInt)

      val comparison = comp match {
        case "==" => Eq
        case "!=" => NotEq
        case "<" => Less
        case "<=" => LessEq
        case ">" => Greater
        case ">=" => GreaterEq
      }

      Instruction(register, operation, Condition(conditionRegister, comparison, conditionAmount.toInt))
  }

  type Instructions = List[Instruction]

  def parseInstructions(instructionsStr: String): Instructions = instructionsStr.linesIterator.map(parseInstruction).toList

  type Registers = Map[Register, Int]

  def evalCondition(condition: Condition, registerAmount: Int): Boolean = {
    condition.comparison match {
      case Eq => registerAmount == condition.amount
      case NotEq => registerAmount != condition.amount
      case Less => registerAmount < condition.amount
      case LessEq => registerAmount <= condition.amount
      case Greater => registerAmount > condition.amount
      case GreaterEq => registerAmount >= condition.amount
    }
  }

  def execOperation(operation: Operation, registerAmount: Int): Int = operation match {
    case Inc(amount) => registerAmount + amount
    case Dec(amount) => registerAmount - amount
  }

  private val defaultAmount = 0

  def run(instructions: Instructions): Iterator[Registers] = {
    instructions.iterator.scanLeft(Map.empty.withDefaultValue(defaultAmount): Registers) { (registers, instruction) =>
      if (evalCondition(instruction.condition, registers(instruction.condition.register)))
        registers.updated(instruction.register, execOperation(instruction.operation, registers(instruction.register)))
      else
        registers
    }
  }

  def largestValueAfter(instructions: Instructions): Int = {
    val registers = run(instructions).last
    registers.values.max
  }

  def largestValueAfter(instructionsStr: String): Int = largestValueAfter(parseInstructions(instructionsStr))

  def largestValueDuring(instructions: Instructions): Int = {
    val registerss = run(instructions)
    registerss.flatMap(_.values.maxOption).max
  }

  def largestValueDuring(instructionsStr: String): Int = largestValueDuring(parseInstructions(instructionsStr))

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(largestValueAfter(input))
    println(largestValueDuring(input))
  }
}
