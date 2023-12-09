package eu.sim642.adventofcode2016

import Day23.SimulatedSolution.Value._
import Day23.SimulatedSolution.Instruction._
import eu.sim642.adventofcodelib.IteratorImplicits._

import scala.util.matching.Regex

object Day23 {

  val part1eggs = 7
  val part2eggs = 12

  trait Solution {
    def safeValue(input: String, eggs: Int): Int
  }

  object SimulatedSolution extends Solution {
    // copied from 2016 day 12
    type Register = Char
    type Integer = Int

    enum Value {
      case ConstValue(const: Integer)
      case RegisterValue(register: Register)
    }

    enum Instruction {
      case Cpy(x: Value, y: Value)
      case Inc(x: Value)
      case Dec(x: Value)
      case Jnz(x: Value, y: Value)
      case Tgl(x: Value)
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

    def safeValue(instructions: Instructions, eggs: Integer): Integer = iterateExec(State(instructions, registers = Map('a' -> eggs))).last.registers('a')

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

    def parseInstructions(input: String): Instructions = input.linesIterator.map(parseInstruction).toVector

    override def safeValue(input: String, eggs: Int): Int = safeValue(parseInstructions(input), eggs)
  }

  object ReverseEngineeredSolution extends Solution {

    lazy val inputRegex: Regex = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day23/reverse_regex.txt")).mkString.trim.r

    def factorial(n: Int): Int = (1 to n).product

    def safeValue(cCpy: Int, dJnz: Int, eggs: Int): Int = {
      factorial(eggs) + cCpy * dJnz
    }

    override def safeValue(input: String, eggs: Int): Int = {
      val m = inputRegex.findFirstMatchIn(input).get
      val cCpy = m.group("cCpy").toInt
      val dJnz = m.group("dJnz").toInt
      safeValue(cCpy, dJnz, eggs)
    }
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import ReverseEngineeredSolution._

    println(safeValue(input, part1eggs))
    println(safeValue(input, part2eggs))
  }
}
