package eu.sim642.adventofcode2018

import scala.collection.mutable

object Day1 {
  def resultingFreq(freqChanges: Seq[Int]): Int = freqChanges.sum

  def firstTwiceFreq(freqChanges: Seq[Int]): Int = {
    val it = Iterator.continually(freqChanges).flatten.scanLeft(0)(_ + _)
    val prevFreqs = mutable.Set[Int]()

    var firstTwice: Option[Int] = None
    do {
      val freq = it.next()
      if (!prevFreqs.add(freq))
        firstTwice = Some(freq)
    } while (firstTwice.isEmpty)

    firstTwice.get
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim
  lazy val inputFreqChanges: Seq[Int] = input.lines.map(_.toInt).toSeq

  def main(args: Array[String]): Unit = {
    println(resultingFreq(inputFreqChanges))
    println(firstTwiceFreq(inputFreqChanges))
  }
}
