package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day23 {

  // copied & modified from 2016 Day 12, inspired by 2017 day 18
  type Register = Char
  type Integer = Int
  type Offset = Int

  sealed trait Instruction
  case class Hlf(r: Register) extends Instruction
  case class Tpl(r: Register) extends Instruction
  case class Inc(r: Register) extends Instruction
  case class Jmp(offset: Offset) extends Instruction
  case class Jie(r: Register, offset: Offset) extends Instruction
  case class Jio(r: Register, offset: Offset) extends Instruction

  type Instructions = Vector[Instruction]
  type Registers = Map[Register, Integer]

  case class State(instructions: Instructions,
                   pc: Int = 0,
                   registers: Registers = Map.empty.withDefaultValue(0)) {
    def instruction: Instruction = instructions(pc)

    def terminated: Boolean = !instructions.indices.contains(pc)
  }

  def execInstruction(state: State): State = {
    val State(instructions, pc, registers) = state

    state.instruction match {
      case Hlf(r) =>
        state.copy(registers = registers + (r -> (registers(r) / 2)), pc = pc + 1)
      case Tpl(r) =>
        state.copy(registers = registers + (r -> (registers(r) * 3)), pc = pc + 1)
      case Inc(r) =>
        state.copy(registers = registers + (r -> (registers(r) + 1)), pc = pc + 1)
      case Jmp(offset) =>
        state.copy(pc = pc + offset)
      case Jie(r, offset) =>
        if (registers(r) % 2 == 0)
          state.copy(pc = pc + offset)
        else
          state.copy(pc = pc + 1)
      case Jio(r, offset) =>
        if (registers(r) == 1)
          state.copy(pc = pc + offset)
        else
          state.copy(pc = pc + 1)
    }
  }

  def iterateExec(initialState: State): Iterator[State] = {
    Iterator.iterate(initialState)(execInstruction).takeWhile(!_.terminated)
  }

  trait Part {
    protected def initialRegisters: Registers

    def execRegisterB(instructions: Instructions): Integer = iterateExec(State(instructions, registers = initialRegisters)).last.registers('b')

    def execRegisterB(input: String): Integer = execRegisterB(parseInstructions(input))
  }

  object Part1 extends Part {
    override protected def initialRegisters: Registers = Map.empty.withDefaultValue(0)
  }

  private val instructionRegex = """([a-z]+) ([a-z]|[+-]\d+)(?:, ([a-z]|[+-]\d+))?""".r

  def parseInstruction(str: String): Instruction = str match {
    case instructionRegex(instruction, x, y) => instruction match {
      case "hlf" => Hlf(x.head)
      case "tpl" => Tpl(x.head)
      case "inc" => Inc(x.head)
      case "jmp" => Jmp(x.toInt)
      case "jie" => Jie(x.head, y.toInt)
      case "jio" => Jio(x.head, y.toInt)
    }
  }

  def parseInstructions(input: String): Instructions = input.linesIterator.map(parseInstruction).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.execRegisterB(input))
  }
}
