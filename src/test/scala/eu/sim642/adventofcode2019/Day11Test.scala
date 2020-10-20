package eu.sim642.adventofcode2019

import org.scalatest.Suites
import intcode.parseProgram
import Day11._
import eu.sim642.adventofcode2019.Day11Test._
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends Suites(
  new KnotTyingSolutionTest,
  new OutputLoopSolutionTest,
)

object Day11Test {

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {

    // manually constructed Intcode program which just outputs the example values
    val exampleInput = "104,1,104,0,104,0,104,0,104,1,104,0,104,1,104,0,104,0,104,1,104,1,104,0,104,1,104,0,99"

    test("Part 1 examples") {
      assert(solution.countPainted(parseProgram(exampleInput)) == 6)
    }

    test("Part 1 input answer") {
      assert(solution.countPainted(parseProgram(input)) == 1863)
    }

    test("Part 2 input answer") {
      solution.renderIdentifier(parseProgram(input)) // BLULZJLZ
    }
  }

  class KnotTyingSolutionTest extends SolutionTest(KnotTyingSolution)

  class OutputLoopSolutionTest extends SolutionTest(OutputLoopSolution)
}
