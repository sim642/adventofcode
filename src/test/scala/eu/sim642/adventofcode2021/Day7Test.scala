package eu.sim642.adventofcode2021

import Day7._
import Day7Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends Suites(
  new BaseTest,
  new LinearSolutionTest,
  new BinarySolutionTest,
  new MathSolutionTest,
)

object Day7Test {

  val exampleInput = "16,1,2,0,4,2,7,1,2,14"

  class BaseTest extends AnyFunSuite {
    test("Part 1 examples") {
      val exampleCrabs = parseCrabs(exampleInput)
      assert(Part1Fuel.alignPosFuel(exampleCrabs, 2) == 37)
      assert(Part1Fuel.alignPosFuel(exampleCrabs, 1) == 41)
      assert(Part1Fuel.alignPosFuel(exampleCrabs, 3) == 39)
      assert(Part1Fuel.alignPosFuel(exampleCrabs, 10) == 71)
    }

    test("Part 2 examples") {
      val exampleCrabs = parseCrabs(exampleInput)
      assert(Part2Fuel.alignPosFuel(exampleCrabs, 5) == 168)
      assert(Part2Fuel.alignPosFuel(exampleCrabs, 2) == 206)
    }
  }

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {
    test("Part 1 examples") {
      assert(solution.Part1.minAlignPosFuel(parseCrabs(exampleInput)) == 37)
    }

    test("Part 1 input answer") {
      assert(solution.Part1.minAlignPosFuel(parseCrabs(input)) == 336721)
    }

    test("Part 2 examples") {
      assert(solution.Part2.minAlignPosFuel(parseCrabs(exampleInput)) == 168)
    }

    test("Part 2 input answer") {
      assert(solution.Part2.minAlignPosFuel(parseCrabs(input)) == 91638945)
    }
  }

  class LinearSolutionTest extends SolutionTest(LinearSolution)

  class BinarySolutionTest extends SolutionTest(BinarySolution)

  class MathSolutionTest extends SolutionTest(MathSolution)
}
