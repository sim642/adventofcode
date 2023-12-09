package eu.sim642.adventofcode2017

import Day18.Value._
import Day18.Instruction._
import scala.collection.immutable.Queue

object Day18 {

  type Register = Char
  type Integer = Long

  enum Value {
    case RegisterValue(register: Register)
    case ConstValue(const: Integer)
  }

  enum Instruction {
    case Snd(x: Value)
    case Set(x: Register, y: Value)
    case Add(x: Register, y: Value)
    case Mul(x: Register, y: Value)
    case Mod(x: Register, y: Value)
    case Rcv(x: Register)
    case Jgz(x: Value, y: Value)
  }

  type Instructions = Vector[Instruction]
  type Registers = Map[Register, Integer]

  case class AsmState(instructions: Instructions,
                      pc: Int = 0,
                      registers: Registers = Map.empty.withDefaultValue(0),
                      snds: Queue[Integer] = Queue.empty,
                      rcvs: Queue[Integer] = Queue.empty) {
    def get(value: Value): Integer = value match {
      case RegisterValue(register) => registers(register)
      case ConstValue(const) => const
    }

    def instruction: Instruction = instructions(pc)

    def terminated: Boolean = !instructions.indices.contains(pc)
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

  def parseInstructions(str: String): Instructions = str.linesIterator.map(parseInstruction).toVector

  trait Part {
    def execSmallStep(state: AsmState): AsmState = {
      val AsmState(instructions, pc, registers, snds, rcvs) = state

      if (state.terminated)
        state
      else {
        state.instruction match {
          case Snd(x) =>
            state.copy(snds = snds.enqueue(state.get(x)), pc = pc + 1)
          case Set(x, y) => state.copy(registers = registers + (x -> state.get(y)), pc = pc + 1)
          case Add(x, y) => state.copy(registers = registers + (x -> (registers(x) + state.get(y))), pc = pc + 1)
          case Mul(x, y) => state.copy(registers = registers + (x -> (registers(x) * state.get(y))), pc = pc + 1)
          case Mod(x, y) => state.copy(registers = registers + (x -> (registers(x) % state.get(y))), pc = pc + 1)
          case Rcv(x) =>
            execRcvSmallStep(state)
          case Jgz(x, y) =>
            if (state.get(x) > 0)
              state.copy(pc = pc + state.get(y).toInt)
            else
              state.copy(pc = pc + 1)
        }
      }
    }

    protected def execRcvSmallStep(state: AsmState): AsmState

    def iterateSmallStep(instructions: Instructions): Iterator[AsmState] = Iterator.iterate(AsmState(instructions))(execSmallStep)
  }

  object Part1 extends Part {
    override protected def execRcvSmallStep(state: AsmState): AsmState = {
      val AsmState(instructions, pc, registers, snds, rcvs) = state
      val Rcv(x) = state.instruction: @unchecked

      if (registers(x) != 0)
        state.copy(registers = registers + (x -> snds.last), pc = pc + 1)
      else
        state.copy(pc = pc + 1)
    }

    def firstRcv(instructions: Instructions): Integer = iterateSmallStep(instructions).find(state => state.instruction match {
      case Rcv(x) => state.registers(x) != 0
      case _ => false
    }).get.snds.last

    def firstRcv(input: String): Integer = firstRcv(parseInstructions(input))
  }

  object Part2 extends Part {
    override protected def execRcvSmallStep(state: AsmState): AsmState = {
      val AsmState(instructions, pc, registers, snds, rcvs) = state
      val Rcv(x) = state.instruction: @unchecked

      rcvs.dequeueOption match {
        case Some((rcv, newRcvs)) =>
          state.copy(registers = registers + (x -> rcv), rcvs = newRcvs, pc = pc + 1)
        case None =>
          state
      }
    }

    type AsmStatePair = (AsmState, AsmState)

    implicit class AsmStatePairExtra(statePair: AsmStatePair) {
      private val (state0, state1) = statePair

      def deadlocked: Boolean = {
        if (state0.terminated || state1.terminated)
          false
        else {
          (state0.instruction, state1.instruction) match {
            case (Rcv(_), Rcv(_)) => state0.rcvs.isEmpty && state1.rcvs.isEmpty
            case _ => false
          }
        }
      }

      def terminated: Boolean = (state0.terminated && state1.terminated) || deadlocked
    }

    def execSmallStepPair(statePair: AsmStatePair): AsmStatePair = {
      val (state0, state1) = statePair
      var (newState0, newState1) = (execSmallStep(state0), execSmallStep(state1))

      val snds0 = newState0.snds
      val snds1 = newState1.snds
      newState0 = newState0.copy(snds = Queue.empty, rcvs = newState0.rcvs.enqueueAll(snds1))
      newState1 = newState1.copy(snds = Queue.empty, rcvs = newState1.rcvs.enqueueAll(snds0))

      (newState0, newState1)
    }

    def iterateSmallStepPair(instructions: Instructions): Iterator[AsmStatePair] =
      Iterator.iterate((
        AsmState(instructions, registers = Map('p' -> 0L).withDefaultValue(0)),
        AsmState(instructions, registers = Map('p' -> 1L).withDefaultValue(0))
      ))(execSmallStepPair)

    def countSnd1(instructions: Instructions): Int = iterateSmallStepPair(instructions).takeWhile(!_.terminated).count(statePair => {
      val state1 = statePair._2
      state1.instruction match {
        case Snd(_) => true
        case _ => false
      }
    })

    def countSnd1(input: String): Int = countSnd1(parseInstructions(input))
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.firstRcv(input))
    println(Part2.countSnd1(input))
  }
}
