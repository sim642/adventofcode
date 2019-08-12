package eu.sim642.adventofcode2016

object Day25 {

  // copied from 2016 day 12
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
  case class Out(x: Value) extends Instruction

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
      case Out(x) =>
        state.copy(pc = pc + 1) // checked externally
    }
  }

  def iterateExec(initialState: State): Iterator[State] = {
    Iterator.iterate(initialState)(execInstruction).takeWhile(!_.terminated)
  }

  def iterateOuts(initialState: State): Iterator[Integer] = {
    iterateExec(initialState).flatMap({ state =>
      state.instruction match {
        case Out(x) => Some(state.get(x))
        case _ => None
      }
    })
  }

  def clockSignalRegisterA(instructions: Instructions): Integer = {
    val clockSignal = Seq(0, 1, 0, 1, 0, 1, 0, 1, 0, 1) // reasonably long prefix from description
    Iterator.from(1).find({ a =>
      val initialRegisters = Map('a' -> a).withDefaultValue(0)
      val outStream = iterateOuts(State(instructions, registers = initialRegisters)).toStream
      //println(s"$a: ${outStream.take(clockSignal.size).toList}")
      outStream.startsWith(clockSignal)
    }).get
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
      case "out" => Out(parseValue(x))
    }
  }

  def parseInstructions(input: String): Instructions = input.lines.map(parseInstruction).toVector

  def clockSignalRegisterA(input: String): Integer = clockSignalRegisterA(parseInstructions(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(clockSignalRegisterA(input))
  }
}
