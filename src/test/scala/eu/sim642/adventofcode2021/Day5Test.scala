package eu.sim642.adventofcode2021

import Day5._
import Day5Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends Suites(
  new NaiveSolutionTest,
  new IntersectSolutionTest,
)

object Day5Test {

  val exampleInput =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {

    test("Part 1 examples") {
      assert(solution.countOverlaps(parseLines(exampleInput)) == 5)
    }

    test("Part 1 input answer") {
      assert(solution.countOverlaps(parseLines(input)) == 5197)
    }

    test("Part 2 examples") {
      assert(solution.countOverlaps(parseLines(exampleInput), true) == 12)
    }

    test("Part 2 input answer") {
      assert(solution.countOverlaps(parseLines(input), true) == 18605)
    }
  }

  class NaiveSolutionTest extends SolutionTest(NaiveSolution)

  class IntersectSolutionTest extends SolutionTest(IntersectSolution)
}
