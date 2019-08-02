package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2017.Day8.LastIterator

object Day12 {

  // inspired by 2017 day 18
  type Register = Char
  type Integer = Int

  sealed trait Value
  case class ConstValue(const: Integer) extends Value
  case class RegisterValue(register: Register) extends Value

  sealed trait Instruction
  case class Cpy(x: Value, y: Register) extends Instruction
  case class Inc(x: Register) extends Instruction
  case class Dec(x: Register) extends Instruction
  case class Jnz(x: Value, y: Value) extends Instruction

  type Instructions = Vector[Instruction]
  type Registers = Map[Register, Integer]

  case class State(instructions: Instructions,
                   pc: Int = 0,
                   registers: Registers = Map.empty.withDefaultValue(0)) {
    def get(value: Value): Integer = value match {
      case ConstValue(const) => const
      case RegisterValue(register) => registers(register)
    }

    def instruction: Instruction = instructions(pc)

    def terminated: Boolean = !instructions.indices.contains(pc)
  }

  def execInstruction(state: State): State = {
    val State(instructions, pc, registers) = state

    state.instruction match {
      case Cpy(x, y) =>
        state.copy(registers = registers + (y -> state.get(x)), pc = pc + 1)
      case Inc(x) =>
        state.copy(registers = registers + (x -> (registers(x) + 1)), pc = pc + 1)
      case Dec(x) =>
        state.copy(registers = registers + (x -> (registers(x) - 1)), pc = pc + 1)
      case Jnz(x, y) =>
        if (state.get(x) != 0)
          state.copy(pc = pc + state.get(y))
        else
          state.copy(pc = pc + 1)
    }
  }

  def iterateExec(instructions: Instructions): Iterator[State] = {
    Iterator.iterate(State(instructions))(execInstruction).takeWhile(!_.terminated)
  }

  def execRegisterA(instructions: Instructions): Integer = iterateExec(instructions).last.registers('a')

  private val instructionRegex = """([a-z]+) ([a-z]|-?\d+)(?: ([a-z]|-?\d+))?""".r

  def parseValue(str: String): Value = {
    val c = str.head
    if (c.isLetter)
      RegisterValue(c)
    else
      ConstValue(str.toInt)
  }

  def parseInstruction(str: String): Instruction = str match {
    case instructionRegex(instruction, x, y) => instruction match {
      case "cpy" => Cpy(parseValue(x), y.head)
      case "inc" => Inc(x.head)
      case "dec" => Dec(x.head)
      case "jnz" => Jnz(parseValue(x), parseValue(y))
    }
  }

  def parseInstructions(input: String): Instructions = input.lines.map(parseInstruction).toVector

  def execRegisterA(input: String): Integer = execRegisterA(parseInstructions(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(execRegisterA(input))
  }
}
