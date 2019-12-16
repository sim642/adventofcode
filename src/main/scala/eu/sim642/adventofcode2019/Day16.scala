package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.IterableImplicits._

object Day16 {

  private val basePattern = Seq(0, 1, 0, -1)

  def stepPhase(signal: IndexedSeq[Int]): IndexedSeq[Int] = {
    val prefixSum: IndexedSeq[Int] = signal.scanLeft(0)(_ + _)

    def posRanges(repeat: Int): Iterator[Range.Inclusive] = {
      Iterator.from(0)
        .map(i => (4 * i * repeat + repeat - 1) to (4 * i * repeat + 2 * repeat - 1 - 1))
    }

    def negRanges(repeat: Int): Iterator[Range.Inclusive] = {
      Iterator.from(0)
        .map(i => (4 * i * repeat + 3 * repeat - 1) to (4 * i * repeat + 4 * repeat - 1 - 1))
    }

    (for {
      (number, i) <- signal.view.zipWithIndex
      repeat = i + 1
      posSum = posRanges(repeat).takeWhile(_.start < signal.length).map(r => prefixSum((r.end + 1) min (prefixSum.length - 1)) - prefixSum(r.start)).sum
      negSum = negRanges(repeat).takeWhile(_.start < signal.length).map(r => prefixSum((r.end + 1) min (prefixSum.length - 1)) - prefixSum(r.start)).sum
      sum = posSum - negSum
      newNumber = (sum % 10).abs
    } yield newNumber).toIndexedSeq
  }

  def stepPhases(signal: IndexedSeq[Int], phases: Int = 100): IndexedSeq[Int] = {
    Iterator.iterate(signal)(stepPhase)(phases)
  }

  def stepPhasesEight(signal: IndexedSeq[Int], phases: Int = 100): String = {
    stepPhases(signal, phases).take(8).mkString
  }

  def stepPhasesEight2(signal: IndexedSeq[Int], phases: Int = 100): String = {
    val messageOffset = signal.take(7).mkString.toInt
    val initialSignal = Iterator.fill(10000)(signal).flatten.toIndexedSeq
    stepPhases(initialSignal, phases).slice(messageOffset, messageOffset + 8).mkString
  }

  def parseSignal(input: String): IndexedSeq[Int] = input.map(_.asDigit)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(stepPhasesEight(parseSignal(input)))
    println(stepPhasesEight2(parseSignal(input)))
  }
}
