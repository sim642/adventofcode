package eu.sim642.adventofcode2025

import eu.sim642.adventofcode2016.Day20.Interval

object Day2 {

  type Range = Interval

  def numDigits(i: Long): Int = {
    i.toString.length // TODO: already have a (better) way to get number of digits?
  }

  def invalidIds(times: Int): LazyList[Long] = LazyList.from(1).map(i =>
    val pow = Math.powExact(10L, numDigits(i))
    val pattern = (1 until times).foldLeft(1L)((acc, _) => pow * acc + 1)
    i * pattern
  )

  trait Part {
    def maxTimes(max: Long): Int

    def sumInvalidIds(ranges: Seq[Range]): Long = {
      val max = ranges.map(_.max).max
      (for {
        times <- 2 to maxTimes(max) // TODO: bad shadowing
        id <- invalidIds(times).takeWhile(_ <= max)
        if ranges.exists(_.contains(id))
      } yield id).toSet.sum // toSet because some IDs can be created multiple ways (2222 is 2-2-2-2 and 22-22), but shouldn't count multiple times
    }
  }

  object Part1 extends Part {
    override def maxTimes(max: Long): Int = 2
  }

  object Part2 extends Part {
    override def maxTimes(max: Long): Int = numDigits(max)
  }

  def parseRange(s: String): Range = s match {
    case s"$i-$j" => Interval(i.toLong, j.toLong)
  }

  def parseRanges(input: String): Seq[Interval] = input.split(",").toSeq.map(parseRange)

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumInvalidIds(parseRanges(input)))
    println(Part2.sumInvalidIds(parseRanges(input)))
  }
}
