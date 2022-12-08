package eu.sim642.adventofcode2022

import Day8.*
import Day8Test.*
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends Suites(
  new NaiveSolutionTest,
  new PrefixSolutionTest,
  new OptimizedPrefixSolutionTest,
)

object Day8Test {

  val exampleInput =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {

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

  trait VisibleIndicesSolutionTest(solution: VisibleIndicesSolution) extends AnyFunSuite {

    test("visibleIndices") {
      import solution.VisibleIndices

      val exampleGrid = parseGrid(exampleInput)
      val visibleIndices = solution.makeVisibleIndices(exampleGrid)

      assert(visibleIndices(Pos(2, 1)) == VisibleIndices(top = -1, left = 1, right = 5, bottom = 3))
      assert(visibleIndices(Pos(2, 3)) == VisibleIndices(top = 1, left = -1, bottom = 5, right = 4))
    }
  }

  class NaiveSolutionTest extends SolutionTest(NaiveSolution) with VisibleIndicesSolutionTest(NaiveSolution)

  class PrefixSolutionTest extends SolutionTest(PrefixSolution) with VisibleIndicesSolutionTest(PrefixSolution)

  class OptimizedPrefixSolutionTest extends SolutionTest(OptimizedPrefixSolution)
}
