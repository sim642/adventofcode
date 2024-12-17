package eu.sim642.adventofcode2024

import com.microsoft.z3.{Context, Status}

import scala.jdk.CollectionConverters.*

object Day17 {

  case class Registers(a: Int, b: Int, c: Int)

  type Program = Seq[Int]

  case class Input(registers: Registers, program: Program)

  def runOutput0(input: Input): Seq[Int] = {
    val Input(registers, program) = input

    def helper(ip: Int, registers: Registers): LazyList[Int] = {
      //println(ip)
      //println(registers)

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
            //helper(ip + 2, registers.copy(a = registers.a / (1 << comboOperand)))
            helper(ip + 2, registers.copy(a = registers.a >> comboOperand))
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
            //helper(ip + 2, registers.copy(b = registers.a / (1 << comboOperand)))
            helper(ip + 2, registers.copy(b = registers.a >> comboOperand))
          case 7 => // cdv
            //helper(ip + 2, registers.copy(c = registers.a / (1 << comboOperand)))
            helper(ip + 2, registers.copy(c = registers.a >> comboOperand))
          case _ => throw new IllegalArgumentException("illegal instruction")
        }
      }
      else
        LazyList.empty
    }

    helper(0, registers)
  }

  def runOutput(input: Input): String = runOutput0(input).mkString(",")

  def findQuineA(input: Input): Int = {
    Iterator.from(0)
      .find(newA => runOutput0(Input(input.registers.copy(a = newA), input.program)) == input.program)
      .get
  }

  def myProg(initialA: Int, expectedOutputs: Iterator[Int]): Boolean = {
    var a: Int = initialA
    var b: Int = 0
    var c: Int = 0
    while (a != 0) {
      b = a & 0b111
      b = b ^ 1
      c = a >> b
      b = b ^ 5
      b = b ^ c
      a = a >> 3
      if ((b & 0b111) != expectedOutputs.next())
        return false
    }
    !expectedOutputs.hasNext
  }

  def findQuineA2(input: Input): Int = {
    Iterator.from(0)
      .find(newA => myProg(newA, input.program.iterator))
      .get
  }

  def findQuineA3(input: Input): Long = {
    val ctx = new Context(Map("model" -> "true").asJava)
    import ctx._
    val s = mkSolver()

    val bits = input.program.size * 3
    val initialA = mkBVConst("initialA", bits)

    for ((instruction, i) <- input.program.zipWithIndex) {
      val a = mkBVLSHR(initialA, mkBV(i * 3, bits))
      var b = mkBVAND(a, mkBV(7, bits))
      b = mkBVXOR(b, mkBV(1, bits))
      val c = mkBVLSHR(a, b)
      b = mkBVXOR(b, mkBV(5, bits))
      b = mkBVXOR(b, c)
      val out = mkBVAND(b, mkBV(7, bits))
      s.add(mkEq(out, mkBV(instruction, bits)))
    }

    assert(s.check() == Status.SATISFIABLE)
    s.getModel.evaluate(initialA, false).toString.toLong
  }

  def parseInput(input: String): Input = input match {
    case s"Register A: $a\nRegister B: $b\nRegister C: $c\n\nProgram: $programStr" =>
      val registers = Registers(a.toInt, b.toInt, c.toInt)
      val program = programStr.split(",").map(_.toInt).toSeq
      Input(registers, program)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    /*var a: Int = 30344604
    var b: Int = 0
    var c: Int = 0
    while (a != 0) {
      b = a & 0b111
      b = b ^ 1
      c = a >> b
      b = b ^ 5
      b = b ^ c
      a = a >> 3
      print(b & 0b111)
      print(',')
    }*/


    //println(runOutput(parseInput(input)))
    println(findQuineA3(parseInput(input)))

    // part 2: 164540892147389 - correct

    // part 1: 4,5,0,4,7,4,3,0,0 - wrong (bst used literal not combo operand)
  }
}
