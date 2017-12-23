package eu.sim642.adventofcode2017

import Day23._
import eu.sim642.adventofcode2017.Day23Test._
import org.scalatest.{FunSuite, Suites}

class Day23Test extends Suites(
  new BaseTest,
  new SimulateSolutionTest,
  new ReverseEngineeredSolutionTest
)

object Day23Test {

  class BaseTest extends FunSuite {
    test("isPrime") {
      assert((0 to 30).filter(isPrime) == Seq(2, 3, 5, 7, 11, 13, 17, 19, 23, 29))
    }
  }

  sealed abstract class SolutionTest(solution: Solution) extends FunSuite {
    test("Part 1 input answer") {
      assert(solution.countMul(input) == 8281)
    }

    test("Part 2 input answer") {
      assert(solution.registerH(input) == 911)
    }
  }

  class SimulateSolutionTest extends SolutionTest(SimulateSolution)

  class ReverseEngineeredSolutionTest extends SolutionTest(ReverseEngineeredSolution)
}
