package eu.sim642.adventofcode2016

import Day12.Value._
import Day12.Instruction._
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day12 {

  // inspired by 2017 day 18
  type Register = Char
  type Integer = Int

  enum Value {
    case ConstValue(const: Integer)
    case RegisterValue(register: Register)
  }

  enum Instruction {
    case Cpy(x: Value, y: Register)
    case Inc(x: Register)
    case Dec(x: Register)
    case Jnz(x: Value, y: Value)
  }

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

  def iterateExec(initialState: State): Iterator[State] = {
    Iterator.iterate(initialState)(execInstruction).takeWhile(!_.terminated)
  }

  trait Part {
    protected def initialRegisters: Registers

    def execRegisterA(instructions: Instructions): Integer = iterateExec(State(instructions, registers = initialRegisters)).last.registers('a')

    def execRegisterA(input: String): Integer = execRegisterA(parseInstructions(input))
  }

  object Part1 extends Part {
    override protected def initialRegisters: Registers = Map.empty.withDefaultValue(0)
  }

  object Part2 extends Part {
    override protected def initialRegisters: Registers = Map('c' -> 1).withDefaultValue(0)
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
      case "cpy" => Cpy(parseValue(x), y.head)
      case "inc" => Inc(x.head)
      case "dec" => Dec(x.head)
      case "jnz" => Jnz(parseValue(x), parseValue(y))
    }
  }

  def parseInstructions(input: String): Instructions = input.linesIterator.map(parseInstruction).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.execRegisterA(input))
    println(Part2.execRegisterA(input)) // didn't require reverse engineering
  }
}
