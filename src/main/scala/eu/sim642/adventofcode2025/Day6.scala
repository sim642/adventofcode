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

  trait Part {
    def parseProblems(input: String): Seq[Problem]
  }

  object Part1 extends Part {
    override def parseProblems(input: String): Seq[Problem] = {
      input
        .linesIterator
        .map(_.trim.split(" +").toSeq)
        .toSeq
        .transpose
        .map(s => Problem(s.init.map(_.toInt), parseProblemKind(s.last)))
    }
  }

  object Part2 extends Part {
    override def parseProblems(input: String): Seq[Problem] = {
      val lines = input.linesIterator.toSeq
      val maxLength = lines.map(_.length).max
      val paddedLines = lines.map(_.padTo(maxLength, ' ')) // pad because test code has trimmed trailing whitespace
      val cols = paddedLines.transpose

      // TODO: split on Seq?
      def helper(cols: Seq[Seq[Char]]): List[Problem] = {
        val (problemCols, newCols) = cols.span(!_.forall(_ == ' '))
        val problemKind = parseProblemKind(problemCols.head.last.toString)
        val nums = problemCols.map(_.init.mkString("").trim.toInt)
        val problem = Problem(nums, problemKind)
        val rest = {
          if (newCols.isEmpty)
            Nil
          else
            helper(newCols.tail)
        }
        problem :: rest
      }

      helper(cols)
    }
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumAnswers(Part1.parseProblems(input)))
    println(sumAnswers(Part2.parseProblems(input)))

    // part 1: 1615951811 - too low (Int overflowed in Problem#answer)
  }
}
