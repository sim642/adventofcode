package eu.sim642.adventofcode2017

object Day13 {

  private val rangeRegex = """(\d+): (\d+)""".r

  def parseRange(line: String): (Int, Int) = line match {
    case rangeRegex(depth, range) => depth.toInt -> range.toInt
  }

  def parseRanges(input: String): Map[Int, Int] = input.linesIterator.map(parseRange).toMap

  // not really needed
  def rangePosition(range: Int, time: Int): Int = {
    val range1 = range - 1
    val q = time / range1
    val r = time % range1
    if (q % 2 == 0)
      r
    else
      range1 - r
  }

  def rangesPositions(ranges: Map[Int, Int], time: Int): Map[Int, Int] = ranges.view.mapValues(rangePosition(_, time)).toMap

  // only needed to check 0
  def rangeCaught(range: Int, time: Int): Boolean = time % (2 * (range - 1)) == 0

  def rangesCaught(ranges: Map[Int, Int], delay: Int = 0): Set[Int] = {
    ranges.filter({ case (depth, range) => rangeCaught(range, delay + depth) }).keySet
  }

  def tripSeverity(ranges: Map[Int, Int]): Int = {
    val caught = rangesCaught(ranges)
    caught.map(depth => depth * ranges(depth)).sum
  }

  def tripSeverity(input: String): Int = tripSeverity(parseRanges(input))

  def uncaughtDelay(ranges: Map[Int, Int]): Int = Stream.from(0).find(delay => rangesCaught(ranges, delay).isEmpty).get

  def uncaughtDelay(input: String): Int = uncaughtDelay(parseRanges(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(tripSeverity(input))
    println(uncaughtDelay(input))
  }
}
