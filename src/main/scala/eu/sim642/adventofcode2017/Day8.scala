package eu.sim642.adventofcode2017

object Day8 {

  type Register = String

  sealed trait Operation
  case class Inc(amount: Int) extends Operation
  case class Dec(amount: Int) extends Operation

  sealed trait Comparison
  case object Eq extends Comparison
  case object NotEq extends Comparison
  case object Less extends Comparison
  case object LessEq extends Comparison
  case object Greater extends Comparison
  case object GreaterEq extends Comparison

  case class Condition(register: Register, comparison: Comparison, amount: Int)

  case class Instruction(register: Register, operation: Operation, condition: Condition)

  private val instructionRegex = """(\w+) (inc|dec) (-?\d+) if (\w+) (==|!=|<|<=|>|>=) (-?\d+)""".r

  def parseInstruction(instructionStr: String): Instruction = instructionStr match {
    case instructionRegex(register, op, amount, conditionRegister, comp, conditionAmount) =>
      val operation = (op match {
        case "inc" => Inc
        case "dec" => Dec
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

  implicit class LastIterator[A](iterator: Iterator[A]) {
    def last: A = iterator.reduce((_, x) => x)
  }

  implicit class OptionIntIterable(iterable: Iterable[Int]) {
    def maxOption: Option[Int] = iterable.reduceOption(_ max _)
  }

  private val defaultAmount = 0

  def run(instructions: Instructions): Iterator[Registers] = {
    instructions.toIterator.scanLeft(Map.empty.withDefaultValue(defaultAmount): Registers) { (registers, instruction) =>
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

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(largestValueAfter(input))
    println(largestValueDuring(input))
  }
}
