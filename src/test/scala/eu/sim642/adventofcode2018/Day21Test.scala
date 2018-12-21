package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2018.Day21._
import eu.sim642.adventofcode2018.Day21Test.{ReverseEngineeredSolutionTest, SimulatedSolutionTest}
import org.scalatest.{FunSuite, Suites}

class Day21Test extends Suites(
  new SimulatedSolutionTest,
  new ReverseEngineeredSolutionTest,
)

object Day21Test {

  sealed abstract class SolutionTest(solution: Solution) extends FunSuite {
    test("Part 1 input answer") {
      assert(solution.firstHaltr0(input) == 13970209)
    }

    protected val testPart2: Boolean = true

    if (testPart2) {
      test("Part 2 input answer") {
        assert(solution.lastHaltr0(input) == 6267260)
      }
    }
  }

  class SimulatedSolutionTest extends SolutionTest(SimulateSolution) {
    override protected val testPart2: Boolean = false // takes ~8 minutes
  }

  class ReverseEngineeredSolutionTest extends SolutionTest(ReverseEngineeredSolution)
}
