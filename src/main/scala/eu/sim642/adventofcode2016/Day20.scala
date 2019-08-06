package eu.sim642.adventofcode2016

object Day20 {

  case class Interval(min: Long, max: Long) { // need Long to represent unsigned Int max
    def contains(value: Long): Boolean = min <= value && value <= max
  }

  def minUnblocked(intervals: Seq[Interval]): Long = {
    intervals
      .map(_.max + 1)
      .sorted
      .find(value =>intervals.forall(!_.contains(value))).get
  }


  def parseInterval(s: String): Interval = {
    val splitLongs = s.split("-").map(_.toLong)
    Interval(splitLongs(0), splitLongs(1))
  }

  def parseInput(input: String): Seq[Interval] = input.lines.map(parseInterval).toSeq

  def minUnblocked(input: String): Long = minUnblocked(parseInput(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(minUnblocked(input))
  }
}
