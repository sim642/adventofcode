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

  test("Part 1 examples") {
    assert(totalFencingPrice(parseGrid(exampleInput)) == 140)
    assert(totalFencingPrice(parseGrid(exampleInput2)) == 772)
    assert(totalFencingPrice(parseGrid(exampleInput3)) == 1930)
  }

  test("Part 1 input answer") {
    assert(totalFencingPrice(parseGrid(input)) == 1433460)
  }
}
