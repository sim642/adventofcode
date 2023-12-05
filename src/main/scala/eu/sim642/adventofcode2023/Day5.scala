package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.IteratorImplicits.*

import scala.annotation.tailrec

object Day5 {

  // TODO: move to library
  case class Interval(min: Long, max: Long) {
    require(min <= max)

    def intersect(that: Interval): Option[Interval] = {
      val intersectMin = min max that.min
      val intersectMax = max min that.max
      if (intersectMin <= intersectMax)
        Some(Interval(intersectMin, intersectMax))
      else
        None
    }

    def diffSplit(that: Interval): Intervals = this intersect that match {
      case None => Set(this)
      case Some(inter) =>

        def make(a: Long, b: Long): Option[Interval] = {
          if (a <= b)
            Some(Interval(a, b))
          else
            None
        }

        Set(
          (min, inter.min - 1),
          (inter.max + 1, max),
        ).flatMap(make)
    }
  }

  object Interval {
    def apply(x: Long): Interval = Interval(x, x)
  }

  type Intervals = Set[Interval]

  case class RangeMapEntry(destination: Long, source: Long, length: Long) extends Function1[Interval, Option[Interval]] {
    val sourceRange: Interval = Interval(source, source + length - 1)

    override def apply(x: Interval): Option[Interval] = x intersect sourceRange match {
      case Some(Interval(start, end)) => Some(Interval(start - source + destination, end - source + destination))
      case None => None
    }
  }

  case class RangeMap(entries: Seq[RangeMapEntry]) extends Function1[Interval, Intervals] {
    override def apply(x: Interval): Intervals = {
      val ranges = entries.flatMap(entry => entry(x)).toSet

      @tailrec
      def helper(ranges: Intervals, entries: Seq[RangeMapEntry]): Intervals = {
        if (entries.isEmpty)
          ranges
        else {
          val newRanges = ranges.flatMap(_.diffSplit(entries.head.sourceRange))
          helper(newRanges, entries.tail)
        }
      }

      ranges ++ helper(Set(x), entries)
    }
  }

  case class Input(seeds: Seq[Long], rangeMaps: Seq[RangeMap]) extends Function1[Interval, Intervals] {
    override def apply(x: Interval): Intervals = {

      @tailrec
      def helper(ranges: Intervals, rangeMaps: Seq[RangeMap]): Intervals = {
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
    input.seeds.flatMap(seed => input(Interval(seed, seed + 1))).map(_.min).min
  }

  def lowestSeedRangeLocation(input: Input): Long = {
    input.seeds.grouped(2).map({ case Seq(start, range) => Interval(start, start + range - 1) }).flatMap(input).map(_.min).min
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
