package eu.sim642.adventofcode2016

object Day20 {

  case class Interval(min: Long, max: Long) { // need Long to represent unsigned Int max
    def contains(value: Long): Boolean = min <= value && value <= max
    def size: Long = max - min + 1
  }

  private val unsignedIntInterval = Interval(0, 4294967295L)

  def mergeIntervals(intervals: Seq[Interval]): Seq[Interval] = {
    val sortedIntervals = intervals.sortBy(_.min)
    sortedIntervals.foldLeft(List.empty[Interval]){
      case (acc@prev :: tl, cur) if cur.min <= prev.max + 1 =>
        /*if (cur.max <= prev.max)
          acc
        else
          Interval(prev.min, cur.max) :: tl*/
        Interval(prev.min, prev.max max cur.max) :: tl
      case (acc, cur) =>
        cur :: acc
    }
  }

  def minUnblocked(intervals: Seq[Interval]): Long = {
    mergeIntervals(intervals).map(_.max + 1).min
  }

  def countUnblocked(intervals: Seq[Interval], validInterval: Interval): Long = {
    validInterval.size - mergeIntervals(intervals).map(_.size).sum
  }


  def parseInterval(s: String): Interval = {
    val splitLongs = s.split("-").map(_.toLong)
    Interval(splitLongs(0), splitLongs(1))
  }

  def parseInput(input: String): Seq[Interval] = input.linesIterator.map(parseInterval).toSeq

  def minUnblocked(input: String): Long = minUnblocked(parseInput(input))

  def countUnblocked(input: String, validInterval: Interval = unsignedIntInterval): Long = countUnblocked(parseInput(input), validInterval)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(minUnblocked(input))
    println(countUnblocked(input))
  }
}
