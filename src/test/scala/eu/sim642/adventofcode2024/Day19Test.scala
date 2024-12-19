package eu.sim642.adventofcode2024

import Day19._
import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  val exampleInput =
    """r, wr, b, g, bwu, rb, gb, br
      |
      |brwrr
      |bggr
      |gbbr
      |rrbgbr
      |ubwu
      |bwurrg
      |brgr
      |bbrgwb""".stripMargin

  test("Part 1 examples") {
    assert(countPossibleDesigns(parseInput(exampleInput)) == 6)
  }

  test("Part 1 input answer") {
    assert(countPossibleDesigns(parseInput(input)) == 226)
  }
}
