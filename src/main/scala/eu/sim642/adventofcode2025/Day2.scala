package eu.sim642.adventofcode2025

import eu.sim642.adventofcode2016.Day20.Interval

object Day2 {

  type Range = Interval

  val invalidIds: LazyList[Long] = LazyList.from(1).map(i =>
    val len = i.toString.length // TODO: already have a (better) way to get number of digits?
    i * (Math.powExact(10L, len) + 1)
  )

  def sumInvalidIds(ranges: Seq[Range]): Long = { // TODO: Long?
    val max = ranges.map(_.max).max
    invalidIds
      .takeWhile(_ <= max)
      .filter(id =>
        ranges.exists(_.contains(id))
      )
      .sum
  }

  def parseRange(s: String): Range = s match {
    case s"$i-$j" => Interval(i.toLong, j.toLong)
  }

  def parseRanges(input: String): Seq[Interval] = input.split(",").toSeq.map(parseRange)

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumInvalidIds(parseRanges(input)))
  }
}
