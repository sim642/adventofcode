package eu.sim642.adventofcode2021

import Day23._
import org.scalatest.funsuite.AnyFunSuite

class Day23Test extends AnyFunSuite {

  val exampleInput =
    """#############
      |#...........#
      |###B#C#B#D###
      |  #A#D#C#A#
      |  #########""".stripMargin

  test("Part 1 examples") {
    assert(Part1.minimumOrganizeEnergy(parseGrid(exampleInput)) == 12521)
  }

  test("Part 1 input answer") {
    assert(Part1.minimumOrganizeEnergy(parseGrid(input)) == 15237)
  }

  test("Part 2 examples") {
    assert(Part2.minimumOrganizeEnergy(parseGrid(exampleInput)) == 44169)
  }

  test("Part 2 input answer") {
    assert(Part2.minimumOrganizeEnergy(parseGrid(input)) == 47509)
  }
}
