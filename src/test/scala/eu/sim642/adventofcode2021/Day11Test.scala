package eu.sim642.adventofcode2021

import Day11._
import Day11Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends Suites(
  new NaiveSolutionTest,
  new DFSSolutionTest,
)

object Day11Test {

  val exampleInput1 =
    """11111
      |19991
      |19191
      |19991
      |11111""".stripMargin

  val exampleInput2 =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {
    test("Part 1 examples") {
      assert(solution.simulateStep(parseGrid(exampleInput1))._1 == parseGrid(
        """34543
          |40004
          |50005
          |40004
          |34543""".stripMargin
      ))

      assert(solution.countFlashes(parseGrid(exampleInput1), 1) == 9)
      assert(solution.countFlashes(parseGrid(exampleInput2), 10) == 204)
      assert(solution.countFlashes(parseGrid(exampleInput2)) == 1656)
    }

    test("Part 1 input answer") {
      assert(solution.countFlashes(parseGrid(input)) == 1773)
    }

    test("Part 2 examples") {
      assert(solution.findSimultaneousFlash(parseGrid(exampleInput2)) == 195)
    }

    test("Part 2 input answer") {
      assert(solution.findSimultaneousFlash(parseGrid(input)) == 494)
    }
  }

  class NaiveSolutionTest extends SolutionTest(NaiveSolution)

  class DFSSolutionTest extends SolutionTest(DFSSolution)
}
