package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2018.Day16.Instruction
import eu.sim642.adventofcode2018.Day16.Registers

object Day19 {

  def runProgram(program: Vector[Instruction], ipRegister: Int): Int = {
    var registers = Seq.fill(6)(0)
    var ip = 0
    while (program.indices.contains(ip)) {
      val beforeRegisters = registers.updated(ipRegister, ip)
      val instruction = program(ip)
      val afterRegisters = instruction(beforeRegisters)
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

  def runProgram(input: String): Int = {
    val (ipRegister, program) = parseInput(input)
    runProgram(program, ipRegister)
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(runProgram(input))
  }
}
