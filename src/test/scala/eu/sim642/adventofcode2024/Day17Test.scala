package eu.sim642.adventofcode2024

import Day17.*
import Day17Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends Suites(
  new Part1Test,
  new NaivePart2SolutionTest,
  new Z3Part2SolutionTest,
)

object Day17Test {

  // TODO: small examples

  val exampleInput =
    """Register A: 729
      |Register B: 0
      |Register C: 0
      |
      |Program: 0,1,5,4,3,0""".stripMargin

  val exampleInput1 =
    """Register A: 10
      |Register B: 0
      |Register C: 0
      |
      |Program: 5,0,5,1,5,4""".stripMargin

  val exampleInput2 =
    """Register A: 2024
      |Register B: 0
      |Register C: 0
      |
      |Program: 0,1,5,4,3,0""".stripMargin

  val exampleInput3 =
    """Register A: 2024
      |Register B: 0
      |Register C: 0
      |
      |Program: 0,3,5,4,3,0""".stripMargin

  class Part1Test extends AnyFunSuite {
    test("Part 1 examples") {
      assert(runOutput(parseInput(exampleInput)) == "4,6,3,5,6,3,5,2,1,0")
      assert(runOutput(parseInput(exampleInput1)) == "0,1,2")
      assert(runOutput(parseInput(exampleInput2)) == "4,2,5,6,7,7,7,7,3,1,0")
    }

    test("Part 1 input answer") {
      assert(runOutput(parseInput(input)) == "4,3,2,6,4,5,3,2,4")
    }
  }

  abstract class Part2SolutionExampleTest(part2Solution: Part2Solution) extends AnyFunSuite {
    test("Part 2 examples") {
      assert(part2Solution.findQuineA(parseInput(exampleInput3)) == 117440)
    }
  }

  abstract class Part2SolutionInputTest(part2Solution: Part2Solution) extends AnyFunSuite {
    test("Part 2 input answer") {
      assert(part2Solution.findQuineA(parseInput(input)) == 164540892147389L)
    }
  }

  class NaivePart2SolutionTest extends Part2SolutionExampleTest(NaivePart2Solution)

  class Z3Part2SolutionTest extends Part2SolutionInputTest(Z3Part2Solution)
}