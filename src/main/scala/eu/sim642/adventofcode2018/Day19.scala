package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2018.Day16.Instruction
import eu.sim642.adventofcode2018.Day16.Registers

object Day19 {

  def runProgram(program: Vector[Instruction], ipRegister: Int, register0: Int): Int = {
    var registers = Seq.fill(6)(0).updated(0, register0)
    var ip = 0
    while (program.indices.contains(ip)) {
      val beforeRegisters = registers.updated(ipRegister, ip)
      val instruction = program(ip)
      val afterRegisters = instruction(beforeRegisters)
      //println(s"$beforeRegisters $instruction $afterRegisters")
      ip = afterRegisters(ipRegister)
      registers = afterRegisters
      ip += 1
    }
    registers(0)
  }

  private val ipRegisterRegex = """#ip (\d)""".r
  private val instructionRegex = """(\w+) (\d+) (\d+) (\d+)""".r

  def parseInstruction(s: String): Instruction = s match {
    case instructionRegex(opcode, a, b, c) => Instruction(opcode, a.toInt, b.toInt, c.toInt)
  }

  def parseInput(input: String): (Int, Vector[Instruction]) = {
    val lines = input.lines.toVector

    val ipRegister = lines.head match {
      case ipRegisterRegex(register) => register.toInt
    }

    val program = lines.tail.map(parseInstruction)

    (ipRegister, program)
  }

  def runProgram(input: String, register0: Int): Int = {
    val (ipRegister, program) = parseInput(input)
    runProgram(program, ipRegister, register0)
  }

  def reverseEngineered(): Int = {
    var r3 = 973
    var r0 = 0
    var r2 = 1
    do {
      var r1 = 1
      do {
        if (r2 * r1 == r3)
          r0 += r2
        r1 += 1
      } while (r1 <= r3)
      r2 += 1
    } while (r2 <= r3)
    r0
  }

  def reverseEngineered2(): Int = {
    val N = 10551373
    //val N = 973
    var sum = 0
    var r1 = 1
    do {
      val r2 = N / r1
      if (r2 * r1 == N)
        sum += r2
      r1 += 1
    } while (r1 <= N)
    sum
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    //println(runProgram(input, 0))
    //println(runProgram(input, 1))
    //println(reverseEngineered())
    println(reverseEngineered2()) // 12768192

    // 40435200 - too high
  }
}
