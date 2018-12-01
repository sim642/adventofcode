package eu.sim642.adventofcode2018

object Day1 {
  def resultingFreq(freqChanges: Seq[Int]): Int = freqChanges.sum

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim
  lazy val inputFreqChanges: Seq[Int] = input.lines.map(_.toInt).toSeq

  def main(args: Array[String]): Unit = {
    println(resultingFreq(inputFreqChanges))
  }
}
