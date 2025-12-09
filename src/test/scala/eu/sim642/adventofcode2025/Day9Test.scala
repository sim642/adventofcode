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

  val largeUInput =
    """1,1
      |3,1
      |3,1000
      |1000,1000
      |1000,1
      |1002,1
      |1002,1002
      |1,1002""".stripMargin

  val adjacentCoordinateInput = // from Timvde on Libera IRC
    """1,1
      |3,1
      |3,6
      |4,6
      |4,1
      |6,1
      |6,10
      |1,10""".stripMargin

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(Part1.largestArea(parseRedTiles(exampleInput)) == 50)
    }

    test("Part 1 large U") {
      assert(Part1.largestArea(parseRedTiles(largeUInput)) == 1004004)
    }

    test("Part 1 adjacent coordinate") {
      assert(Part1.largestArea(parseRedTiles(adjacentCoordinateInput)) == 60)
    }

    test("Part 1 input answer") {
      assert(Part1.largestArea(parseRedTiles(input)) == 4729332959L)
    }
  }

  abstract class Part2SolutionTest(val part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      assert(part2Solution.largestArea(parseRedTiles(exampleInput)) == 24)
    }

    ignore("Part 2 adjacent coordinate") {
      // should not be 30
      assert(part2Solution.largestArea(parseRedTiles(adjacentCoordinateInput)) == 60)
    }

    test("Part 2 input answer") {
      assert(part2Solution.largestArea(parseRedTiles(input)) == 1474477524L)
    }
  }

  class CompressGridPart2SolutionTest extends Part2SolutionTest(CompressGridPart2Solution) {

    test("Part 2 large U") {
      // should not be 998000, which would be the completely outside box in the large U
      assert(part2Solution.largestArea(parseRedTiles(largeUInput)) == 3006)
    }
  }

  class IntersectionPart2SolutionTest extends Part2SolutionTest(IntersectionPart2Solution) {

    ignore("Part 2 large U") { // fails due to assumption
      // should not be 998000, which would be the completely outside box in the large U
      assert(part2Solution.largestArea(parseRedTiles(largeUInput)) == 3006)
    }
  }
}
