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

  val opcodes = Set("addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir", "gtri", "gtrr", "eqir", "eqri", "eqrr")

  case class InputInstruction(opcode: Int, a: Int, b: Int, c: Int)

  case class Sample(before: Registers, instruction: InputInstruction, after: Registers)

  def sampleOpcodes(sample: Sample): Set[String] = {
    opcodes.filter({ opcode =>
      val InputInstruction(_, a, b, c) = sample.instruction
      val instruction = Instruction(opcode, a, b, c)
      instruction(sample.before) == sample.after
    })
  }

  def countSampleOpcodes(sample: Sample): Int = {
    sampleOpcodes(sample).size
  }

  private val sampleRegex =
    """Before: \[(\d+), (\d+), (\d+), (\d+)\]
      |(\d+) (\d+) (\d+) (\d+)
      |After:  \[(\d+), (\d+), (\d+), (\d+)\]""".stripMargin.r

  def parseSample(s: String): Sample = s match {
    case sampleRegex(before0, before1, before2, before3, opcode, a, b, c, after0, after1, after2, after3) => 
      Sample(
        Seq(before0, before1, before2, before3).map(_.toInt),
        InputInstruction(opcode.toInt, a.toInt, b.toInt, c.toInt),
        Seq(after0, after1, after2, after3).map(_.toInt)
      )
  }

  def parseInputInstruction(s: String): InputInstruction = {
    val Seq(opcode, a, b, c) = s.split(" ").toSeq.map(_.toInt)
    InputInstruction(opcode, a, b, c)
  }

  def parseInput(input: String): (Seq[Sample], Seq[InputInstruction]) = {
    val (samples, program) = input.splitAt(input.indexOf("\n\n\n\n"))
    (samples.split("""\n\n""").map(parseSample), program.trim.lines.map(parseInputInstruction).toSeq)
  }

  def count3Samples(samples: Seq[Sample]): Int = {
    samples.count(countSampleOpcodes(_) >= 3)
  }

  def count3Samples(input: String): Int = {
    val (samples, _) = parseInput(input)
    count3Samples(samples)
  }

  def fitSamples(samples: Seq[Sample]): Map[Int, String] = {
    val opcodeMap = samples.foldLeft(Map.empty[Int, Set[String]].withDefaultValue(opcodes))({ (opcodeMap, sample) =>
      val ops = sampleOpcodes(sample)
      val ops2 = opcodeMap(sample.instruction.opcode) intersect ops
      val map2 = opcodeMap.updated(sample.instruction.opcode, ops2)
      if (ops2.size == 1)
        map2.mapValues(o => if (o == ops2) o else o - ops2.head).withDefaultValue(opcodes)
      else
        map2
    })
    println(opcodeMap)
    opcodeMap.mapValues(_.head)
  }

  def runProgram(program: Seq[InputInstruction], opcodeMap: Map[Int, String]): Int = {
    program.foldLeft(Seq(0, 0, 0, 0))({ case (registers, InputInstruction(opcode, a, b, c)) =>
      val instruction = Instruction(opcodeMap(opcode), a, b, c)
      instruction(registers)
    })(0)
  }

  def runProgram(input: String): Int = {
    val (samples, program) = parseInput(input)
    val opcodeMap = fitSamples(samples)
    runProgram(program, opcodeMap)
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(count3Samples(input))
    println(runProgram(input))

    // 2
  }
}
