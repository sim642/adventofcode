package eu.sim642.adventofcode2022

import Day8.*
import Day8Test.*
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends Suites(
  new NaiveSolutionTest,
  new PrefixSolutionTest,
)

object Day8Test {
  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {

    val exampleInput =
      """30373
        |25512
        |65332
        |33549
        |35390""".stripMargin

    test("Part 1 examples") {
      assert(solution.countVisibleTrees(parseGrid(exampleInput)) == 21)
    }

    test("Part 1 input answer") {
      assert(solution.countVisibleTrees(parseGrid(input)) == 1713)
    }

    test("Part 2 examples") {
      /*val exampleGrid = parseGrid(exampleInput)
      val exampleGridTranspose = exampleGrid.transpose
      assert(scenicScore(exampleGrid, exampleGridTranspose, Pos(2, 1)) == 4)
      assert(scenicScore(exampleGrid, exampleGridTranspose, Pos(2, 3)) == 8)*/

      assert(solution.maxScenicScore(parseGrid(exampleInput)) == 8)
    }

    test("Part 2 input answer") {
      assert(solution.maxScenicScore(parseGrid(input)) == 268464)
    }
  }

  class NaiveSolutionTest extends SolutionTest(NaiveSolution)

  class PrefixSolutionTest extends SolutionTest(PrefixSolution)
}
