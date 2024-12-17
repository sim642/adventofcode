package eu.sim642.adventofcode2024

import com.microsoft.z3.{BitVecExpr, BoolExpr, Context, Status}

import scala.annotation.tailrec
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
            (comboOperand & 0b111) #:: helper(ip + 2, registers)
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

  trait Part2Solution {
    def findQuineA(input: Input): Long
  }

  object NaivePart2Solution extends Part2Solution {
    override def findQuineA(input: Input): Long = {
      Iterator.from(0)
        .find(newA => runOutput0(Input(input.registers.copy(a = newA), input.program)) == input.program)
        .get
    }
  }

  object SemiNaivePart2Solution extends Part2Solution {
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

    override def findQuineA(input: Input): Long = {
      Iterator.from(0)
        .find(newA => myProg(newA, input.program.iterator))
        .get
    }
  }

  object ReverseEngineeredZ3Part2Solution extends Part2Solution {
    override def findQuineA(input: Input): Long = {
      val Seq(2,4,1,bxl1,7,5,1,bxl2,4,5,0,3,5,5,3,0) = input.program

      val ctx = new Context(Map("model" -> "true").asJava)
      import ctx._
      val s = mkSolver()

      val bits = input.program.size * 3
      val initialA = mkBVConst("initialA", bits)

      for ((instruction, i) <- input.program.zipWithIndex) {
        val a = mkBVLSHR(initialA, mkBV(i * 3, bits))
        var b = mkBVAND(a, mkBV(0b111, bits))
        b = mkBVXOR(b, mkBV(bxl1, bits))
        val c = mkBVLSHR(a, b)
        b = mkBVXOR(b, mkBV(bxl2, bits))
        b = mkBVXOR(b, c)
        val out = mkBVAND(b, mkBV(0b111, bits))
        s.add(mkEq(out, mkBV(instruction, bits)))
      }

      assert(s.check() == Status.SATISFIABLE)
      // TODO: minimize
      s.getModel.evaluate(initialA, false).toString.toLong
    }
  }

  /**
   * Inspired by https://github.com/glguy/advent/blob/main/solutions/src/2024/17.hs.
   */
  object GenericZ3Part2Solution extends Part2Solution {
    override def findQuineA(input: Input): Long = {
      val ctx = new Context(Map("model" -> "true").asJava)
      import ctx._
      val s = mkOptimize()

      case class Registers(a: BitVecExpr, b: BitVecExpr, c: BitVecExpr)

      val Input(registers, program) = input
      val bits = input.program.size * 3

      val zeroBV = mkBV(0, bits)
      val threeBitBV = mkBV(0b111, bits)

      // copied & modified from part 1
      def helper(ip: Int, registers: Registers, expectedOutputs: List[Int]): BoolExpr = {

        def combo(operand: Int): BitVecExpr = operand match {
          case 0 | 1 | 2 | 3 => mkBV(operand, bits)
          case 4 => registers.a
          case 5 => registers.b
          case 6 => registers.c
          case 7 => throw new IllegalArgumentException("illegal combo operand")
        }

        if (program.indices.contains(ip)) {
          lazy val literalOperand = mkBV(program(ip + 1), bits)
          lazy val comboOperand = combo(program(ip + 1))

          program(ip) match {
            case 0 => // adv
              helper(ip + 2, registers.copy(a = mkBVLSHR(registers.a, comboOperand)), expectedOutputs)
            case 1 => // bxl
              helper(ip + 2, registers.copy(b = mkBVXOR(registers.b, literalOperand)), expectedOutputs)
            case 2 => // bst
              helper(ip + 2, registers.copy(b = mkBVAND(comboOperand, threeBitBV)), expectedOutputs)
            case 3 => // jnz
              mkOr(
                mkAnd(mkEq(registers.a, zeroBV), helper(ip + 2, registers, expectedOutputs)),
                mkAnd(mkDistinct(registers.a, zeroBV), helper(program(ip + 1), registers, expectedOutputs))
              )
            case 4 => // bxc
              helper(ip + 2, registers.copy(b = mkBVXOR(registers.b, registers.c)), expectedOutputs)
            case 5 => // out
              expectedOutputs match {
                case Nil => mkFalse()
                case expectedOutput :: newExpectedOutputs =>
                  mkAnd(
                    mkEq(mkBVAND(comboOperand, threeBitBV), mkBV(expectedOutput, bits)),
                    helper(ip + 2, registers, newExpectedOutputs)
                  )
              }
            case 6 => // bdv
              helper(ip + 2, registers.copy(b = mkBVLSHR(registers.a, comboOperand)), expectedOutputs)
            case 7 => // cdv
              helper(ip + 2, registers.copy(c = mkBVLSHR(registers.a, comboOperand)), expectedOutputs)
            case _ => throw new IllegalArgumentException("illegal instruction")
          }
        }
        else {
          expectedOutputs match {
            case Nil => mkTrue()
            case _ :: _ => mkFalse()
          }
        }
      }

      val initialA = mkBVConst("initialA", bits)
      val constraint = helper(0, Registers(initialA, zeroBV, zeroBV), program.toList)
      s.Add(constraint)
      s.MkMinimize(initialA)

      assert(s.Check() == Status.SATISFIABLE)
      s.getModel.evaluate(initialA, false).toString.toLong
    }
  }

  object ReverseEngineeredPart2Solution extends Part2Solution {
    override def findQuineA(input: Input): Long = {
      val iterProgram :+ 3 :+ 0 = input.program

      @tailrec
      def helper(as: Set[Long], expectedOutputsRev: List[Int]): Set[Long] = expectedOutputsRev match {
        case Nil => as
        case expectedOutput :: newExpectedOutputsRev =>
          val newAs =
            for {
              a <- as
              aStep <- 0 to 7
              newA = (a << 3) | aStep
              out = runOutput0(Input(Registers((newA & 0b1111111111).toInt, 0, 0), iterProgram))
              if out == Seq(expectedOutput)
            } yield newA
          helper(newAs, newExpectedOutputsRev)
      }

      val as = helper(Set(0), input.program.reverse.toList)
      //as.foreach(println)
      as.min
    }
  }

  def parseInput(input: String): Input = input match {
    case s"Register A: $a\nRegister B: $b\nRegister C: $c\n\nProgram: $programStr" =>
      val registers = Registers(a.toInt, b.toInt, c.toInt)
      val program = programStr.split(",").map(_.toInt).toSeq
      Input(registers, program)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import GenericZ3Part2Solution._
    println(runOutput(parseInput(input)))
    println(findQuineA(parseInput(input)))

    // part 1: 4,5,0,4,7,4,3,0,0 - wrong (bst used literal not combo operand)
  }
}
