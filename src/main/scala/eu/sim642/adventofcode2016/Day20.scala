package eu.sim642.adventofcode2016

object Day20 {

  case class Interval(min: Long, max: Long) { // need Long to represent unsigned Int max
    def contains(value: Long): Boolean = min <= value && value <= max
    def size: Long = max - min + 1
  }

  private val unsignedIntInterval = Interval(0, 4294967295L)

  def minUnblocked(intervals: Seq[Interval]): Long = {
    intervals
      .map(_.max + 1)
      .sorted
      .find(value => intervals.forall(!_.contains(value))).get
  }

  def countUnblocked(intervals: Seq[Interval], validInterval: Interval): Long = {

    def helper(sweep: Long, intervals: Seq[Interval], totalUnblockedSize: Long): Long = {
      if (sweep >= validInterval.max)
        totalUnblockedSize
      else {
        // like minUnblocked but assumes sortedness
        val unblockedMin = intervals.iterator
          .map(_.max + 1)
          .find(value => intervals.forall(!_.contains(value))).get
        val intervals2 = intervals.filter(_.min >= unblockedMin)
        val unblockedMax = {
          if (intervals2.isEmpty)
            validInterval.max
          else
            intervals2.iterator.map(_.min - 1).min
        }
        val unblocked = Interval(unblockedMin, unblockedMax)
        helper(unblockedMax, intervals2, totalUnblockedSize + unblocked.size)
      }
    }

    helper(validInterval.min, intervals.sortBy(_.max), 0)
  }


  def parseInterval(s: String): Interval = {
    val splitLongs = s.split("-").map(_.toLong)
    Interval(splitLongs(0), splitLongs(1))
  }

  def parseInput(input: String): Seq[Interval] = input.lines.map(parseInterval).toSeq

  def minUnblocked(input: String): Long = minUnblocked(parseInput(input))

  def countUnblocked(input: String, validInterval: Interval = unsignedIntInterval): Long = countUnblocked(parseInput(input), validInterval)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(minUnblocked(input))
    println(countUnblocked(input))
  }
}
