package eu.sim642.adventofcode2019

import Day5.parseProgram

object Day7 {

  // copied & modified from 2019 Day 5

  type Memory = Vector[Int]

  case class ProgramState(memory: Memory,
                          inputs: LazyList[Int],
                          ip: Int = 0) {

    def instruction(i: Int): Int = memory(ip + i)
    def opcode: Int = instruction(0) % 100
    def param(i: Int): Int = instruction(i + 1)
    def paramMode(i: Int): Int = (instruction(0) / math.pow(10, 2 + i).toInt) % 10 // TODO: Int pow
    def readParam(i: Int): Int = paramMode(i) match {
      case 0 => memory(param(i))
      case 1 => param(i)
      case _ => throw new IllegalArgumentException(s"Illegal parameter read mode ${paramMode(i)}")
    }
    def writeParam(i: Int, value: Int): Memory = paramMode(i) match {
      case 0 => memory.updated(param(i), value)
      case _ => throw new IllegalArgumentException(s"Illegal parameter write mode ${paramMode(i)}")
    }

    def execOne: Option[(ProgramState, Option[Int])] = {
      opcode match {
        case 1 => // add
          val newValue = readParam(0) + readParam(1)
          val newMemory = writeParam(2, newValue)
          Some((copy(memory = newMemory, ip = ip + 4)), None)
        case 2 => // multiply
          val newValue = readParam(0) * readParam(1)
          val newMemory = writeParam(2, newValue)
          Some((copy(memory = newMemory, ip = ip + 4)), None)
        case 3 => // input
          val (input #:: newInputs) = inputs
          val newMemory = writeParam(0, input)
          Some((copy(memory = newMemory, ip = ip + 2, inputs = newInputs)), None)
        case 4 => // output
          val newValue = readParam(0)
          Some((copy(ip = ip + 2)), Some(newValue))
        case 5 => // jump if true
          if (readParam(0) != 0)
            Some((copy(ip = readParam(1))), None)
          else
            Some((copy(ip = ip + 3)), None)
        case 6 => // jump if false
          if (readParam(0) == 0)
            Some((copy(ip = readParam(1))), None)
          else
            Some((copy(ip = ip + 3)), None)
        case 7 => // less than
          val newValue = if (readParam(0) < readParam(1)) 1 else 0
          val newMemory = writeParam(2, newValue)
          Some((copy(memory = newMemory, ip = ip + 4)), None)
        case 8 => // equal
          val newValue = if (readParam(0) == readParam(1)) 1 else 0
          val newMemory = writeParam(2, newValue)
          Some((copy(memory = newMemory, ip = ip + 4)), None)
        case 99 => None
        case _ => throw new IllegalArgumentException("Unknown opcode")
      }
    }

    def outputs: LazyList[Int] = {
      LazyList.unfold(this)(_.execOne.map({ case (a, b) => (b, a) })).flatten
    }
  }

  trait Part {
    def execPhaseSettingSequence(program: Memory, phaseSettings: Seq[Int]): Int
    val phaseSettings: Seq[Int]

    def findMaxSignal(program: Memory): Int = {
      phaseSettings.permutations
        .map(execPhaseSettingSequence(program, _))
        .max
    }
  }

  object Part1 extends Part {
    def execPhaseSetting(program: Memory, phaseSetting: Int, input: Int): Int = {
      ProgramState(program, LazyList(phaseSetting, input)).outputs.head
    }

    override def execPhaseSettingSequence(program: Memory, phaseSettings: Seq[Int]): Int = {
      phaseSettings.foldLeft(0)({ (signal, phaseSetting) =>
        execPhaseSetting(program, phaseSetting, signal)
      })
    }

    override val phaseSettings: Seq[Int] = 0 to 4
  }

  object Part2 extends Part {
    override def execPhaseSettingSequence(program: Memory, phaseSettings: Seq[Int]): Int = {
      // TODO: generalize somehow?
      assert(phaseSettings.size == 5)

      def a: LazyList[Int] = ProgramState(program, phaseSettings(0) #:: 0 #:: e).outputs
      def b: LazyList[Int] = ProgramState(program, phaseSettings(1) #:: a).outputs
      def c: LazyList[Int] = ProgramState(program, phaseSettings(2) #:: b).outputs
      def d: LazyList[Int] = ProgramState(program, phaseSettings(3) #:: c).outputs
      def e: LazyList[Int] = ProgramState(program, phaseSettings(4) #:: d).outputs

      e.last
    }

    override val phaseSettings: Seq[Int] = 5 to 9
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.findMaxSignal(parseProgram(input)))
    println(Part2.findMaxSignal(parseProgram(input)))
  }
}
