package eu.sim642.adventofcode2025

import Day9.*
import Day9Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends Suites(
  new Part1Test,
  new CompressGridPart2SolutionTest,
  new IntersectionPart2SolutionTest,
)

object Day9Test {

  val exampleInput =
    """7,1
      |11,1
      |11,7
      |9,7
      |9,5
      |2,5
      |2,3
      |7,3""".stripMargin

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(Part1.largestArea(parseRedTiles(exampleInput)) == 50)
    }

    test("Part 1 input answer") {
      assert(Part1.largestArea(parseRedTiles(input)) == 4729332959L)
    }
  }

  abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      assert(part2Solution.largestArea(parseRedTiles(exampleInput)) == 24)
    }

    test("Part 2 input answer") {
      assert(part2Solution.largestArea(parseRedTiles(input)) == 1474477524L)
    }
  }

  class CompressGridPart2SolutionTest extends Part2SolutionTest(CompressGridPart2Solution)

  class IntersectionPart2SolutionTest extends Part2SolutionTest(IntersectionPart2Solution)
}
