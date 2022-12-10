package eu.sim642.adventofcode2022

object Day10 {

  enum Instruction(val cycles: Int) {
    case Addx(v: Int) extends Instruction(2)
    case Noop extends Instruction(1)
  }

  case class CPU(instructions: Seq[Instruction], instructionCycle: Int = 0, x: Int = 1) {

    def tick: CPU = {
      val newInstructionCycle = instructionCycle + 1
      val instruction = instructions.head
      if (newInstructionCycle == instruction.cycles) { // complete instruction
        val newCpu = instruction match {
          case Instruction.Addx(v) => copy(x = x + v)
          case Instruction.Noop => this
        }
        newCpu.copy(instructions = instructions.tail, instructionCycle = 0)
      }
      else
        copy(instructionCycle = newInstructionCycle)
    }

    def run(): Iterator[CPU] = Iterator.iterate(this)(_.tick)
  }

  def sumSignalStrengths(instructions: Seq[Instruction]): Int = {
    val cpus = CPU(instructions).run().take(220).toIndexedSeq
    (20 to 220 by 40).map(i => i * cpus(i - 1).x).sum
  }

  def render(instructions: Seq[Instruction]): Unit = {
    val cpus = CPU(instructions).run().take(240)
    for ((cpu, i) <- cpus.zipWithIndex) {
      val x = i % 40
      //val y = i / 40
      if ((x - cpu.x).abs <= 1)
        print('#')
      else
        print('.')
      Console.flush()

      if (x == 39)
        println()
    }
  }


  def parseInstruction(s: String): Instruction = s match {
    case s"addx $v" => Instruction.Addx(v.toInt)
    case "noop" => Instruction.Noop
  }

  def parseInstructions(input: String): Seq[Instruction] = input.linesIterator.map(parseInstruction).toSeq


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumSignalStrengths(parseInstructions(input)))
    render(parseInstructions(input)) // EJCFPGLH
  }
}
