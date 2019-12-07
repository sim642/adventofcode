package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day5._

object Day7 {

  def execPhaseSetting(program: Memory, phaseSetting: Int, input: Int): Int = {
    execInputs(program, LazyList(phaseSetting, input)).head
  }

  def execPhaseSettingSequence(program: Memory, phaseSettings: Seq[Int]): Int = {
    phaseSettings.foldLeft(0)({ (signal, phaseSetting) =>
      execPhaseSetting(program, phaseSetting, signal)
    })
  }

  def findMaxSignal(program: Memory): Int = {
    (0 to 4).permutations
      .map(execPhaseSettingSequence(program, _))
      .max
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findMaxSignal(parseProgram(input)))
  }
}
