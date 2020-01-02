package eu.sim642.adventofcode2019

import Intcode._

object Day7 {

  trait Part {
    def execPhaseSettingSequence(program: Memory, phaseSettings: Seq[Int]): Int
    val phaseSettings: Seq[Int]

    def findMaxSignal(program: Memory): Int = {
      phaseSettings.permutations
        .map(execPhaseSettingSequence(program, _))
        .max
    }
  }

  object Part1 extends Part {
    override def execPhaseSettingSequence(program: Memory, phaseSettings: Seq[Int]): Int = {
      phaseSettings.foldLeft(0L)({ (signal, phaseSetting) =>
        ProgramState(program, LazyList(phaseSetting, signal)).outputs.head
      }).toInt
    }

    override val phaseSettings: Seq[Int] = 0 to 4
  }

  object Part2 extends Part {
    override def execPhaseSettingSequence(program: Memory, phaseSettings: Seq[Int]): Int = {
      // my initial manual knot tying
      /*assert(phaseSettings.size == 5)

      lazy val a: LazyList[Value] = ProgramState(program, phaseSettings(0) #:: 0L #:: e).outputs
      lazy val b: LazyList[Value] = ProgramState(program, phaseSettings(1) #:: a).outputs
      lazy val c: LazyList[Value] = ProgramState(program, phaseSettings(2) #:: b).outputs
      lazy val d: LazyList[Value] = ProgramState(program, phaseSettings(3) #:: c).outputs
      lazy val e: LazyList[Value] = ProgramState(program, phaseSettings(4) #:: d).outputs

      e.last.toInt*/

      // knot tying generalization of Part 1's foldLeft
      // inspired by: https://github.com/glguy/advent2019/blob/053a95904b1b48cfdc72d6e8f258f9ea778eee16/execs/Day07.hs
      lazy val outputs: LazyList[Value] = phaseSettings.foldLeft(0L #:: outputs)({ (inputs, phaseSetting) =>
        ProgramState(program, phaseSetting #:: inputs).outputs
      })

      outputs.last.toInt
    }

    override val phaseSettings: Seq[Int] = 5 to 9
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.findMaxSignal(parseProgram(input)))
    println(Part2.findMaxSignal(parseProgram(input)))
  }
}
