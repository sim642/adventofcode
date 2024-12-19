package eu.sim642.adventofcode2024

import scala.collection.mutable
import scala.util.matching.Regex

object Day19 {

  type Towel = String

  case class Input(patterns: Seq[Towel], designs: Seq[Towel])

  def patternsRegex(patterns: Seq[Towel]): Regex = {
    val one = patterns.mkString("|")
    s"^($one)*$$".r
  }

  def countPossibleDesigns(input: Input): Int = {
    val regex = patternsRegex(input.patterns)
    input.designs.count(regex.matches)
  }

  def countWays(patterns: Seq[Towel], design: Towel): Long = {
    val memo = mutable.Map.empty[Int, Long]

    def helper(i: Int): Long = {
      memo.getOrElseUpdate(i, {
        if (i == design.size)
          1
        else {
          (for {
            pattern <- patterns
            if design.startsWith(pattern, i)
          } yield helper(i + pattern.size)).sum
        }
      })
    }

    helper(0)
  }

  def sumDesignWays(input: Input): Long =
    input.designs.map(countWays(input.patterns, _)).sum

  def parsePatterns(s: String): Seq[Towel] = s.split(", ").toSeq

  def parseDesigns(s: String): Seq[Towel] = s.linesIterator.toSeq

  def parseInput(input: String): Input = input match {
    case s"$patterns\n\n$designs" => Input(parsePatterns(patterns), parseDesigns(designs))
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countPossibleDesigns(parseInput(input)))
    println(sumDesignWays(parseInput(input)))

    // part 2: 1006410103498494 - too high (had i >= design.size - 1 in countWays)
  }
}
