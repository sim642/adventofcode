package eu.sim642.adventofcode2018

import scala.language.implicitConversions

object Day16 {

  type Registers = Seq[Int]

  case class Instruction(opcode: String, a: Int, b: Int, c: Int) {
    def apply(registers: Registers): Registers = {

      implicit def boolean2Int(b: Boolean): Int = if (b) 1 else 0

      val value: Int = opcode match {
        case "addr" => registers(a) + registers(b)
        case "addi" => registers(a) + b
        case "mulr" => registers(a) * registers(b)
        case "muli" => registers(a) * b
        case "banr" => registers(a) & registers(b)
        case "bani" => registers(a) & b
        case "borr" => registers(a) | registers(b)
        case "bori" => registers(a) | b
        case "setr" => registers(a)
        case "seti" => a
        case "gtir" => a > registers(b)
        case "gtri" => registers(a) > b
        case "gtrr" => registers(a) > registers(b)
        case "eqir" => a == registers(b)
        case "eqri" => registers(a) == b
        case "eqrr" => registers(a) == registers(b)
      }

      registers.updated(c, value)
    }
  }

  val opcodes = Seq("addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr")

  case class Sample(before: Registers, opcode: Int, a: Int, b: Int, c: Int, after: Registers)

  def countSampleOpcodes(sample: Sample): Int = {
    val sampleOpcodes = opcodes.filter({ opcode =>
      val instruction = Instruction(opcode, sample.a, sample.b, sample.c)
      instruction(sample.before) == sample.after
    })
    //println(sampleOpcodes)
    sampleOpcodes.size
  }

  private val sampleRegex =
    """Before: \[(\d+), (\d+), (\d+), (\d+)\]
      |(\d+) (\d+) (\d+) (\d+)
      |After:  \[(\d+), (\d+), (\d+), (\d+)\]""".stripMargin.r

  def parseSample(s: String): Sample = s match {
    case sampleRegex(before0, before1, before2, before3, opcode, a, b, c, after0, after1, after2, after3) => 
      Sample(
        Seq(before0, before1, before2, before3).map(_.toInt),
        opcode.toInt, a.toInt, b.toInt, c.toInt,
        Seq(after0, after1, after2, after3).map(_.toInt)
      )
  }

  def parseInput(input: String): Seq[Sample] = {
    val (samples, program) = input.splitAt(input.indexOf("\n\n\n\n"))
    samples.split("""\n\n""").map(parseSample)
  }

  def count3Samples(samples: Seq[Sample]): Int = {
    samples.count(countSampleOpcodes(_) >= 3)
  }

  def count3Samples(input: String): Int = {
    val samples = parseInput(input)
    count3Samples(samples)
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(count3Samples(input))
  }
}
