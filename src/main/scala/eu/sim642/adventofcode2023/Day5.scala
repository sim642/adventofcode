package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day5 {

  case class RangeMapEntry(destination: Long, source: Long, length: Long) extends PartialFunction[Long, Long] {
    override def isDefinedAt(x: Long): Boolean = source <= x && x < source + length

    override def apply(x: Long): Long = destination + (x - source)
  }

  case class RangeMap(entries: Seq[RangeMapEntry]) extends Function1[Long, Long] {
    override def apply(x: Long): Long = {
      val f = entries.foldRight(PartialFunction.fromFunction[Long, Long](identity))(_ orElse _)
      //println(s"$x -> ${f(x)}")
      f(x)
    }
  }

  case class Input(seeds: Seq[Long], rangeMaps: Seq[RangeMap]) extends Function1[Long, Long] {
    override def apply(x: Long): Long = {
      val f = rangeMaps.foldRight[Long => Long](identity)(_ andThen _)
      f(x)
    }
  }

  def lowestSeedLocation(input: Input): Long = {
    input.seeds.map(input).min
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
  }
}
