package eu.sim642.adventofcode2024

import Day4._
import org.scalatest.funsuite.AnyFunSuite

class Day4Test extends AnyFunSuite {

  val exampleInput =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin

  test("Part 1 examples") {
    assert(countXMAS(parseGrid(exampleInput)) == 18)
  }

  test("Part 1 input answer") {
    assert(countXMAS(parseGrid(input)) == 2454)
  }
}