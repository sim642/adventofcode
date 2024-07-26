package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.IteratorImplicits.*

import scala.annotation.tailrec

object Day5 {

  // TODO: move to library
  case class Interval(min: Long, max: Long) {
    require(min <= max)

    infix def intersect(that: Interval): Option[Interval] = {
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

    override def apply(x: Interval): Option[Interval] = {
      (x intersect sourceRange).map({ case Interval(min, max) =>
        Interval(min - source + destination, max - source + destination)
      })
    }
  }

  case class RangeMap(entries: Seq[RangeMapEntry]) extends Function1[Interval, Intervals] {
    override def apply(x: Interval): Intervals = {
      val mapped = entries.flatMap(_(x)).toSet
      val unmapped = entries.foldLeft(Set(x))((acc, entry) => acc.flatMap(_.diffSplit(entry.sourceRange)))
      mapped ++ unmapped
    }
  }

  case class Input(seeds: Seq[Long], rangeMaps: Seq[RangeMap]) extends Function1[Interval, Intervals] {
    override def apply(x: Interval): Intervals =
      rangeMaps.foldLeft(Set(x))(_.flatMap(_))
  }

  def lowestSeedLocation(input: Input): Long = {
    input.seeds
      .map(Interval.apply)
      .flatMap(input)
      .map(_.min)
      .min
  }

  def lowestSeedRangeLocation(input: Input): Long = {
    input.seeds
      .grouped(2)
      .map({ case Seq(start, length) => Interval(start, start + length - 1) })
      .flatMap(input)
      .map(_.min)
      .min
  }


  def parseRangeMapEntry(s: String): RangeMapEntry = s match {
    case s"$destination $source $length" => RangeMapEntry(destination.toLong, source.toLong, length.toLong)
  }

  def parseRangeMap(s: String): RangeMap = s match {
    //case s"$name map:\n$entries" => // TODO: doesn't work due to Scala bug: https://github.com/scala/bug/issues/12893
    case s =>
      RangeMap(s.linesIterator.tail.map(parseRangeMapEntry).toSeq)
  }

  def parseInput(input: String): Input = {
    val seedsStr +: rangeMapStrs = input.split("\n\n").toSeq: @unchecked
    val seeds = seedsStr match {
      case s"seeds: $seeds" => seeds.split(' ').map(_.toLong).toSeq
    }
    val rangeMaps = rangeMapStrs.map(parseRangeMap)
    Input(seeds, rangeMaps)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(lowestSeedLocation(parseInput(input)))
    println(lowestSeedRangeLocation(parseInput(input)))
  }
}
