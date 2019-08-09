package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2017.Day8.LastIterator

object Day23 {

  // copied from 2016 day 12
  type Register = Char
  type Integer = Int

  sealed trait Value
  case class ConstValue(const: Integer) extends Value
  case class RegisterValue(register: Register) extends Value

  sealed trait Instruction
  case class Cpy(x: Value, y: Value) extends Instruction
  case class Inc(x: Value) extends Instruction
  case class Dec(x: Value) extends Instruction
  case class Jnz(x: Value, y: Value) extends Instruction
  case class Tgl(x: Value) extends Instruction

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

  def toggleInstruction(instruction: Instruction): Instruction = instruction match {
    case Inc(x) => Dec(x)
    case Dec(x) => Inc(x)
    case Tgl(x) => Inc(x)
    case Jnz(x, y) => Cpy(x, y)
    case Cpy(x, y) => Jnz(x, y)
  }

  def execInstruction(state: State): State = {
    val State(instructions, pc, registers) = state

    state.instruction match {
      case Cpy(x, RegisterValue(y)) =>
        state.copy(registers = registers + (y -> state.get(x)), pc = pc + 1)
      case Inc(RegisterValue(x)) =>
        state.copy(registers = registers + (x -> (registers(x) + 1)), pc = pc + 1)
      case Dec(RegisterValue(x)) =>
        state.copy(registers = registers + (x -> (registers(x) - 1)), pc = pc + 1)
      case Jnz(x, y) =>
        if (state.get(x) != 0)
          state.copy(pc = pc + state.get(y))
        else
          state.copy(pc = pc + 1)
      case Tgl(x) =>
        val togglePc = pc + state.get(x)
        if (instructions.indices.contains(togglePc))
          state.copy(instructions = instructions.updated(togglePc, toggleInstruction(instructions(togglePc))), pc = pc + 1)
        else
          state.copy(pc = pc + 1)
      case _ => // invalid instruction
        state.copy(pc = pc + 1)
    }
  }

  def iterateExec(initialState: State): Iterator[State] = {
    Iterator.iterate(initialState)(execInstruction).takeWhile(!_.terminated)
  }

  trait Part {
    protected def initialRegisters: Registers

    def execRegisterA(instructions: Instructions): Integer = iterateExec(State(instructions, registers = initialRegisters)).last.registers('a')

    def execRegisterA(input: String): Integer = execRegisterA(parseInstructions(input))
  }

  object Part1 extends Part {
    override protected def initialRegisters: Registers = Map('a' -> 7).withDefaultValue(0)
  }

  object Part2 extends Part {
    override protected def initialRegisters: Registers = Map('a' -> 12).withDefaultValue(0)
  }

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
      case "cpy" => Cpy(parseValue(x), parseValue(y))
      case "inc" => Inc(parseValue(x))
      case "dec" => Dec(parseValue(x))
      case "jnz" => Jnz(parseValue(x), parseValue(y))
      case "tgl" => Tgl(parseValue(x))
    }
  }

  def parseInstructions(input: String): Instructions = input.lines.map(parseInstruction).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.execRegisterA(input))
    println(Part2.execRegisterA(input)) // didn't require reverse engineering
  }
}
