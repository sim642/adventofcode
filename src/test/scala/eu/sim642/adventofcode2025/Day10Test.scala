package eu.sim642.adventofcode2025

import Day10.*
import Day10Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends Suites(
  new Part1Test,
  new NaivePart2SolutionTest,
  //new Z3Part2SolutionTest,
  new GaussianEliminationPart2SolutionTest,
)

object Day10Test {

  val exampleInput =
    """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
      |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(Part1.sumFewestPresses(parseMachines(exampleInput)) == 7)
    }

    test("Part 1 input answer") {
      assert(Part1.sumFewestPresses(parseMachines(input)) == 449)
    }
  }

  abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      assert(part2Solution.sumFewestPresses(parseMachines(exampleInput)) == 33)
    }

    protected val testInput = true

    if (testInput) {
      test("Part 2 input answer") {
        assert(part2Solution.sumFewestPresses(parseMachines(input)) == 17848)
      }
    }
  }

  class NaivePart2SolutionTest extends Part2SolutionTest(NaivePart2Solution) {
    override protected val testInput: Boolean = false
  }

  class Z3Part2SolutionTest extends Part2SolutionTest(Z3Part2Solution)

  class GaussianEliminationPart2SolutionTest extends Part2SolutionTest(GaussianEliminationPart2Solution)
}
