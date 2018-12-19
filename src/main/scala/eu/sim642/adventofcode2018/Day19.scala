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

  def runProgram(input: String, register0: Int = 0): Int = {
    val (ipRegister, program) = parseInput(input)
    runProgram(program, ipRegister, register0)
  }

  /**
    * Reverse engineered logic
    */
  def sumDivisors(N: Int): Int = {
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
  // Reverse engineered values
  val inputN1 = 973
  val inputN2 = 10551373

  def main(args: Array[String]): Unit = {
    println(runProgram(input))
    println(sumDivisors(inputN2))

    // 40435200 - too high
  }
}
