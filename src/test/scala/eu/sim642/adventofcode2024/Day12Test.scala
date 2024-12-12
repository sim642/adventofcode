package eu.sim642.adventofcode2024

import Day12.*
import Day12Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends Suites(
  new Part1Test,
  new SidesPart2SolutionTest,
  new CornersPart2SolutionTest,
)

object Day12Test {

  val exampleInput =
    """AAAA
      |BBCD
      |BBCC
      |EEEC""".stripMargin

  val exampleInput2 =
    """OOOOO
      |OXOXO
      |OOOOO
      |OXOXO
      |OOOOO""".stripMargin

  val exampleInput3 =
    """RRRRIICCFF
      |RRRRIICCCF
      |VVRRRCCFFF
      |VVRCCCJFFF
      |VVVVCJJCFE
      |VVIVCCJJEE
      |VVIIICJJEE
      |MIIIIIJJEE
      |MIIISIJEEE
      |MMMISSJEEE""".stripMargin

  val exampleInput4 =
    """EEEEE
      |EXXXX
      |EEEEE
      |EXXXX
      |EEEEE""".stripMargin

  val exampleInput5 =
    """AAAAAA
      |AAABBA
      |AAABBA
      |ABBAAA
      |ABBAAA
      |AAAAAA""".stripMargin

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(Part1.totalFencingPrice(parseGrid(exampleInput)) == 140)
      assert(Part1.totalFencingPrice(parseGrid(exampleInput2)) == 772)
      assert(Part1.totalFencingPrice(parseGrid(exampleInput3)) == 1930)
    }

    test("Part 1 input answer") {
      assert(Part1.totalFencingPrice(parseGrid(input)) == 1433460)
    }
  }

  abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      assert(part2Solution.totalFencingPrice(parseGrid(exampleInput)) == 80)
      assert(part2Solution.totalFencingPrice(parseGrid(exampleInput2)) == 436)
      assert(part2Solution.totalFencingPrice(parseGrid(exampleInput4)) == 236)
      assert(part2Solution.totalFencingPrice(parseGrid(exampleInput5)) == 368)
      assert(part2Solution.totalFencingPrice(parseGrid(exampleInput3)) == 1206)
    }

    test("Part 2 input answer") {
      assert(part2Solution.totalFencingPrice(parseGrid(input)) == 855082)
    }
  }

  class SidesPart2SolutionTest extends Part2SolutionTest(SidesPart2Solution)

  class CornersPart2SolutionTest extends Part2SolutionTest(CornersPart2Solution)
}
