package eu.sim642.adventofcode2016

import org.scalatest.Suites
import Day23._
import eu.sim642.adventofcode2016.Day23Test.{ReverseEngineeredSolutionTest, SimulatedSolutionTest}
import org.scalatest.funsuite.AnyFunSuite

class Day23Test extends Suites(
  new SimulatedSolutionTest,
  new ReverseEngineeredSolutionTest,
)

object Day23Test {

  val exampleInput =
    """cpy 2 a
      |tgl a
      |tgl a
      |tgl a
      |cpy 1 a
      |dec a
      |dec a""".stripMargin

  sealed abstract class SolutionTest(val solution: Solution) extends AnyFunSuite {
    test("Part 1 input answer") {
      assert(solution.safeValue(input, part1eggs) == 11893)
    }

    protected val testPart2: Boolean = true

    if (testPart2) {
      test("Part 2 input answer") {
        assert(solution.safeValue(input, part2eggs) == 479008453)
      }
    }
  }

  class SimulatedSolutionTest extends SolutionTest(SimulatedSolution) {
    test("Part 1 examples") {
      assert(solution.safeValue(exampleInput, part1eggs) == 3)
    }

    override protected val testPart2: Boolean = false // takes ~5 minutes
  }

  class ReverseEngineeredSolutionTest extends SolutionTest(ReverseEngineeredSolution)
}
