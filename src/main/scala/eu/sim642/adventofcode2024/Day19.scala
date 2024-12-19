package eu.sim642.adventofcode2024

import scala.util.matching.Regex

object Day19 {

  type Towel = String

  case class Input(patterns: Seq[Towel], designs: Seq[Towel])

  def patternsRegex(patterns: Seq[Towel]): Regex = {
    val one = patterns.mkString("|")
    ("^(" + one + ")*$").r // TODO: why s"..." doesn't work?
  }

  def countPossibleDesigns(input: Input): Int = {
    val regex = patternsRegex(input.patterns)
    input.designs.count(regex.matches)
  }

  def parsePatterns(s: String): Seq[Towel] = s.split(", ").toSeq

  def parseDesigns(s: String): Seq[Towel] = s.linesIterator.toSeq

  def parseInput(input: String): Input = input match {
    case s"$patterns\n\n$designs" => Input(parsePatterns(patterns), parseDesigns(designs))
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countPossibleDesigns(parseInput(input)))
  }
}
