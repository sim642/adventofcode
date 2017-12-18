package eu.sim642.adventofcode2017

object Day18 {

  type Register = Char
  type Integer = Long

  sealed trait Value
  case class RegisterValue(register: Register) extends Value
  case class ConstValue(const: Integer) extends Value

  sealed trait Instruction
  case class Snd(x: Value) extends Instruction
  case class Set(x: Register, y: Value) extends Instruction
  case class Add(x: Register, y: Value) extends Instruction
  case class Mul(x: Register, y: Value) extends Instruction
  case class Mod(x: Register, y: Value) extends Instruction
  case class Rcv(x: Register) extends Instruction
  case class Jgz(x: Value, y: Value) extends Instruction

  type Instructions = Vector[Instruction]
  type Registers = Map[Register, Integer]

  case class AsmState(instructions: Instructions, pc: Int = 0, registers: Registers = Map.empty.withDefaultValue(0), lastSnd: Option[Integer] = None) {
    def get(value: Value): Integer = value match {
      case RegisterValue(register) => registers(register)
      case ConstValue(const) => const
    }

    def instruction: Instruction = instructions(pc)
  }

  private val instructionRegex = """([a-z]+) ([a-z]|-?\d+)(?: ([a-z]|-?\d+))?""".r

  def parseValue(str: String): Value = {
    val c = str.head
    if (c.isLetter)
      RegisterValue(c)
    else
      ConstValue(str.toLong)
  }

  def parseInstruction(str: String): Instruction = str match {
    case instructionRegex(instruction, x, y) => instruction match {
      case "snd" => Snd(parseValue(x))
      case "set" => Set(x.head, parseValue(y))
      case "add" => Add(x.head, parseValue(y))
      case "mul" => Mul(x.head, parseValue(y))
      case "mod" => Mod(x.head, parseValue(y))
      case "rcv" => Rcv(x.head)
      case "jgz" => Jgz(parseValue(x), parseValue(y))
    }
  }

  def parseInstructions(str: String): Instructions = str.lines.map(parseInstruction).toVector

  def execSmallStep(state: AsmState): AsmState = {
    val AsmState(instructions, pc, registers, lastSnd) = state

    state.instruction match {
      case Snd(x) =>
        state.copy(lastSnd = Some(state.get(x)), pc = pc + 1)
      case Set(x, y) => state.copy(registers = registers + (x -> state.get(y)), pc = pc + 1)
      case Add(x, y) => state.copy(registers = registers + (x -> (registers(x) + state.get(y))), pc = pc + 1)
      case Mul(x, y) => state.copy(registers = registers + (x -> (registers(x) * state.get(y))), pc = pc + 1)
      case Mod(x, y) => state.copy(registers = registers + (x -> (registers(x) % state.get(y))), pc = pc + 1)
      case Rcv(x) =>
        if (registers(x) != 0)
          state.copy(registers = registers + (x -> lastSnd.get), pc = pc + 1)
        else
          state.copy(pc = pc + 1)
      case Jgz(x, y) =>
        if (state.get(x) > 0)
          state.copy(pc = pc + state.get(y).toInt)
        else
          state.copy(pc = pc + 1)
    }
  }

  def iterateSmallStep(instructions: Instructions): Iterator[AsmState] = Iterator.iterate(AsmState(instructions))(execSmallStep)

  def firstRcv(instructions: Instructions): Integer = iterateSmallStep(instructions).find(state => state.instruction match {
    case Rcv(x) => state.registers(x) != 0
    case _ => false
  }).get.lastSnd.get

  def firstRcv(input: String): Integer = firstRcv(parseInstructions(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(firstRcv(input))
  }
}
