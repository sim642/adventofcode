package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day5 {

  // copied & modified from 2019 Day 2

  type Memory = Vector[Int]

  case class ProgramState(memory: Memory,
                          inputs: LazyList[Int],
                          outputs: LazyList[Int] = LazyList.empty,
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

    def execOne: Option[ProgramState] = {
      opcode match {
        case 1 => // add
          val newValue = readParam(0) + readParam(1)
          val newMemory = writeParam(2, newValue)
          Some(copy(memory = newMemory, ip = ip + 4))
        case 2 => // multiply
          val newValue = readParam(0) * readParam(1)
          val newMemory = writeParam(2, newValue)
          Some(copy(memory = newMemory, ip = ip + 4))
        case 3 => // input
          val (input #:: newInputs) = inputs
          val newMemory = writeParam(0, input)
          Some(copy(memory = newMemory, ip = ip + 2, inputs = newInputs))
        case 4 => // output
          val newValue = readParam(0)
          val newOutputs = newValue #:: outputs
          Some(copy(ip = ip + 2, outputs = newOutputs))
        case 5 => // jump if true
          if (readParam(0) != 0)
            Some(copy(ip = readParam(1)))
          else
            Some(copy(ip = ip + 3))
        case 6 => // jump if false
          if (readParam(0) == 0)
            Some(copy(ip = readParam(1)))
          else
            Some(copy(ip = ip + 3))
        case 7 => // less than
          val newValue = if (readParam(0) < readParam(1)) 1 else 0
          val newMemory = writeParam(2, newValue)
          Some(copy(memory = newMemory, ip = ip + 4))
        case 8 => // equal
          val newValue = if (readParam(0) == readParam(1)) 1 else 0
          val newMemory = writeParam(2, newValue)
          Some(copy(memory = newMemory, ip = ip + 4))
        case 99 => None
        case _ => throw new IllegalArgumentException("Unknown opcode")
      }
    }

    def execIterator: Iterator[ProgramState] = {
      Iterator.unfold0(this)(_.execOne)
    }

    def execFinal: ProgramState = {
      execIterator.last
    }
  }

  def execInputs(program: Memory, inputs: LazyList[Int]): LazyList[Int] = {
    val finalState = ProgramState(program, inputs).execFinal
    finalState.outputs
  }

  def execDiagnostic(program: Memory, systemId: Int): Int = {
    val (diagnostic #:: outputs) = execInputs(program, LazyList(systemId))
    assert(outputs.forall(_ == 0))
    diagnostic
  }

  def parseProgram(input: String): Memory = input.split(',').toVector.map(_.toInt)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(execDiagnostic(parseProgram(input), 1))
    println(execDiagnostic(parseProgram(input), 5))
  }
}
