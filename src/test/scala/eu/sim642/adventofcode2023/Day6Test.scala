package eu.sim642.adventofcode2023

import Day6.*
import Day6Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends Suites(
  new NaiveSolutionTest,
  new QuadraticSolutionTest,
)

object Day6Test {

  private val exampleInput =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin


  abstract class SolutionTest(solution: Solution) extends AnyFunSuite {

    test("Part 1 examples") {
      val races = parseRaces(exampleInput)

      assert(solution.raceWins(races(0)) == 4)
      assert(solution.raceWins(races(1)) == 8)
      assert(solution.raceWins(races(2)) == 9)

      assert(solution.multiplyRaceWins(races) == 288)
    }

    test("Part 1 input answer") {
      assert(solution.multiplyRaceWins(parseRaces(input)) == 2756160)
    }

    test("Part 2 examples") {
      assert(solution.concatenatedRaceWin(parseRaces(exampleInput)) == 71503)
    }

    test("Part 2 input answer") {
      assert(solution.concatenatedRaceWin(parseRaces(input)) == 34788142)
    }
  }

  class NaiveSolutionTest extends SolutionTest(NaiveSolution)
  class QuadraticSolutionTest extends SolutionTest(QuadraticSolution)
}
