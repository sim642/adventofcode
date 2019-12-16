package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.IterableImplicits._

object Day16 {

  private val basePattern = Seq(0, 1, 0, -1)

  def stepPhase(signal: Seq[Int]): Seq[Int] = {
    // TODO: optimize
    (for {
      (number, i) <- signal.view.zipWithIndex
      repeat = i + 1
      pattern = basePattern.view.flatMap(Iterator.fill(repeat)(_))
      sum = (signal.view zip pattern.cycle.drop(1)).map({ case (a, b) => a * b}).sum
      newNumber = (sum % 10).abs
    } yield newNumber).toSeq
  }

  def stepPhases(signal: Seq[Int], phases: Int = 100): Seq[Int] = {
    Iterator.iterate(signal)(stepPhase)(phases)
  }

  def stepPhasesEight(signal: Seq[Int], phases: Int = 100): String = {
    stepPhases(signal, phases).take(8).mkString
  }

  def parseSignal(input: String): Seq[Int] = input.map(_.asDigit)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(stepPhasesEight(parseSignal(input)))
  }
}
