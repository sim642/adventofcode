package eu.sim642.adventofcode2024

object Day17 {

  case class Registers(a: Int, b: Int, c: Int)

  type Program = Seq[Int]

  case class Input(registers: Registers, program: Program)

  def runOutput(input: Input): String = {
    val Input(registers, program) = input

    def helper(ip: Int, registers: Registers): LazyList[Int] = {
      println(ip)
      println(registers)

      def combo(operand: Int): Int = operand match {
        case 0 | 1 | 2 | 3 => operand
        case 4 => registers.a
        case 5 => registers.b
        case 6 => registers.c
        case 7 => throw new IllegalArgumentException("illegal combo operand")
      }

      // 0, 5, 3
      // 2 1 7 4

      if (program.indices.contains(ip)) {
        lazy val literalOperand = program(ip + 1)
        lazy val comboOperand = combo(literalOperand)

        program(ip) match {
          case 0 => // adv
            helper(ip + 2, registers.copy(a = registers.a / (1 << comboOperand)))
          case 1 => // bxl
            helper(ip + 2, registers.copy(b = registers.b ^ literalOperand))
          case 2 => // bst
            helper(ip + 2, registers.copy(b = comboOperand & 0b111))
          case 3 => // jnz
            helper(if (registers.a != 0) literalOperand else ip + 2, registers)
          case 4 => // bxc
            helper(ip + 2, registers.copy(b = registers.b ^ registers.c))
          case 5 => // out
            (comboOperand & 0b111) +: helper(ip + 2, registers)
          case 6 => // bdv
            helper(ip + 2, registers.copy(b = registers.a / (1 << comboOperand)))
          case 7 => // cdv
            helper(ip + 2, registers.copy(c = registers.a / (1 << comboOperand)))
          case _ => throw new IllegalArgumentException("illegal instruction")
        }
      }
      else
        LazyList.empty
    }

    helper(0, registers).mkString(",")
  }

  def parseInput(input: String): Input = input match {
    case s"Register A: $a\nRegister B: $b\nRegister C: $c\n\nProgram: $programStr" =>
      val registers = Registers(a.toInt, b.toInt, c.toInt)
      val program = programStr.split(",").map(_.toInt).toSeq
      Input(registers, program)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(runOutput(parseInput(input)))

    // part 1: 4,5,0,4,7,4,3,0,0 - wrong (bst used literal not combo operand)
  }
}
