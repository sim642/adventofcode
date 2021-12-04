package eu.sim642.adventofcode2021

import Day4._
import Day4Test._
import org.apache.commons.compress.compressors.xz.XZCompressorInputStream
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

import java.net.URL

class Day4Test extends Suites(
  new SetSolutionTest,
  new IndexSolutionTest,
)

object Day4Test {

  val exampleInput =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7""".stripMargin

  lazy val tkBigInput: String = io.Source.fromInputStream(new XZCompressorInputStream(new URL("https://the-tk.com/files/4-900-15.in.xz").openStream())).mkString.trim

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {
    test("Part 1 examples") {
      assert(solution.firstWinScore(parseInput(exampleInput)) == 4512)
    }

    test("Part 1 input answer") {
      assert(solution.firstWinScore(parseInput(input)) == 44088)
    }

    test("Part 1 tk big") {
      assert(solution.firstWinScore(parseInput(tkBigInput)) == 22010880)
    }

    test("Part 2 examples") {
      assert(solution.lastWinScore(parseInput(exampleInput)) == 1924)
    }

    test("Part 2 input answer") {
      assert(solution.lastWinScore(parseInput(input)) == 23670)
    }

    test("Part 2 tk big") {
      assert(solution.lastWinScore(parseInput(tkBigInput)) == 5371020)
    }
  }

  class SetSolutionTest extends SolutionTest(SetSolution)

  class IndexSolutionTest extends SolutionTest(IndexSolution)
}
