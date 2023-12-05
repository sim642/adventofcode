package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.IteratorImplicits.*

import scala.annotation.tailrec

object Day5 {

  case class Range(start: Long, length: Long) { // TODO: overrides Scala Range
    def end: Long = start + length - 1

    def intersect(that: Range): Option[Range] = {
      val newStart = start max that.start
      val newEnd = end min that.end
      if (newStart <= newEnd)
        Some(Range(newStart, newEnd - newStart + 1))
      else
        None
    }

    def diff(that: Range): Ranges = {
      val r1 =
        if (start < that.start)
          Set(Range(start, length min (that.start - start)))
        else
          Set()
      val r2 =
        if (that.end < end) {
          val newStart = start max (that.end + 1)
          Set(Range(newStart, end - newStart + 1))
        }
        else
          Set()
      r1 ++ r2
    }
  }

  type Ranges = Set[Range]

  case class RangeMapEntry(destination: Long, source: Long, length: Long) extends Function1[Range, Option[Range]] {
    val sourceRange: Range = Range(source, length)

    override def apply(x: Range): Option[Range] = x intersect sourceRange match {
      case Some(Range(start, length)) => Some(Range(start - source + destination, length))
      case None => None
    }
  }

  case class RangeMap(entries: Seq[RangeMapEntry]) extends Function1[Range, Ranges] {
    override def apply(x: Range): Ranges = {
      val ranges = entries.flatMap(entry => entry(x)).toSet

      @tailrec
      def helper(ranges: Ranges, entries: Seq[RangeMapEntry]): Ranges = {
        if (entries.isEmpty)
          ranges
        else {
          val newRanges = ranges.flatMap(_.diff(entries.head.sourceRange))
          helper(newRanges, entries.tail)
        }
      }

      ranges ++ helper(Set(x), entries)
    }
  }

  case class Input(seeds: Seq[Long], rangeMaps: Seq[RangeMap]) extends Function1[Range, Ranges] {
    override def apply(x: Range): Ranges = {

      @tailrec
      def helper(ranges: Ranges, rangeMaps: Seq[RangeMap]): Ranges = {
        if (rangeMaps.isEmpty)
          ranges
        else {
          val newRanges = ranges.flatMap(r => rangeMaps.head(r))
          helper(newRanges, rangeMaps.tail)
        }
      }

      helper(Set(x), rangeMaps)
    }
  }

  def lowestSeedLocation(input: Input): Long = {
    input.seeds.flatMap(seed => input(Range(seed, 1))).map(_.start).min
  }

  def lowestSeedRangeLocation(input: Input): Long = {
    input.seeds.grouped(2).map({ case Seq(start, range) => Range(start, range) }).flatMap(input).map(_.start).min
  }


  def parseRangeMapEntry(s: String): RangeMapEntry = s match {
    case s"$destination $source $length" => RangeMapEntry(destination.toLong, source.toLong, length.toLong)
  }

  def parseRangeMap(s: String): RangeMap = s match {
    //case s"$name map:\n$entries" =>
    case entries => // TODO: why doesn't match work?
      RangeMap(s.linesIterator.tail.map(parseRangeMapEntry).toSeq)
  }

  def parseInput(input: String): Input = {
    val seedsStr +: rangeMapStrs = input.split("\n\n").toSeq // TODO: fix warning
    val seeds = seedsStr match {
      case s"seeds: $seeds" => seeds.split(' ').map(_.toLong).toSeq
    }
    val rangeMaps = rangeMapStrs.map(parseRangeMap)
    Input(seeds, rangeMaps)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(lowestSeedLocation(parseInput(input)))
    println(lowestSeedRangeLocation(parseInput(input)))
  }
}
