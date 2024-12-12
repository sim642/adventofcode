package eu.sim642.adventofcode2024

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

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

  test("Part 1 examples") {
    assert(Part1.totalFencingPrice(parseGrid(exampleInput)) == 140)
    assert(Part1.totalFencingPrice(parseGrid(exampleInput2)) == 772)
    assert(Part1.totalFencingPrice(parseGrid(exampleInput3)) == 1930)
  }

  test("Part 1 input answer") {
    assert(Part1.totalFencingPrice(parseGrid(input)) == 1433460)
  }

  test("Part 2 examples") {
    assert(Part2.totalFencingPrice(parseGrid(exampleInput)) == 80)
    assert(Part2.totalFencingPrice(parseGrid(exampleInput2)) == 436)
    assert(Part2.totalFencingPrice(parseGrid(exampleInput4)) == 236)
    assert(Part2.totalFencingPrice(parseGrid(exampleInput5)) == 368)
    assert(Part2.totalFencingPrice(parseGrid(exampleInput3)) == 1206)
  }

  test("Part 2 input answer") {
    assert(Part2.totalFencingPrice(parseGrid(input)) == 855082)
  }
}
