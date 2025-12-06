package eu.sim642.adventofcode2025

import Day6.ProblemKind._

object Day6 {

  enum ProblemKind {
    case Add
    case Multiply
  }

  case class Problem(nums: Seq[Int], problemKind: ProblemKind) {
    def answer: Long = problemKind match {
      case Add => nums.map(_.toLong).sum
      case Multiply => nums.map(_.toLong).product
    }
  }

  def sumAnswers(problems: Seq[Problem]): Long = {
    problems.map(_.answer).sum
  }

  def parseProblemKind(s: String): ProblemKind = s match {
    case "+" => Add
    case "*" => Multiply
  }

  def parseProblems(input: String): Seq[Problem] = {
    input
      .linesIterator
      .map(_.trim.split(" +").toSeq)
      .toSeq
      .transpose
      .map(s => Problem(s.init.map(_.toInt), parseProblemKind(s.last)))
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumAnswers(parseProblems(input)))

    // part 1: 1615951811 - too low (Int overflowed in Problem#answer)
  }
}
