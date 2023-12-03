package eu.sim642.adventofcode2023

import Day3._
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {

  private val exampleInput =
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin

  test("Part 1 examples") {
    assert(sumPartNumbers(parseSchematic(exampleInput)) == 4361)
  }

  test("Part 1 input answer") {
    assert(sumPartNumbers(parseSchematic(input)) == 521515)
  }

  test("Part 2 examples") {
    assert(sumGearRatios(parseSchematic(exampleInput)) == 467835)
  }

  test("Part 2 input answer") {
    assert(sumGearRatios(parseSchematic(input)) == 69527306)
  }
}
