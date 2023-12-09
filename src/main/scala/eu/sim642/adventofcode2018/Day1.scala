package eu.sim642.adventofcode2018

import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.IterableImplicits._

object Day1 {
  def resultingFreq(freqChanges: Seq[Int]): Int = freqChanges.sum

  def firstTwiceFreq(freqChanges: Seq[Int]): Int = {
    val it = freqChanges.cycle.scanLeft(0)(_ + _)
    NaiveCycleFinder.find(it).get.cycleHead
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim
  lazy val inputFreqChanges: Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  def main(args: Array[String]): Unit = {
    println(resultingFreq(inputFreqChanges))
    println(firstTwiceFreq(inputFreqChanges))
  }
}
