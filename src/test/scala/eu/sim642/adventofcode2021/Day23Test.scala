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

  // TODO: restore
  ignore("Part 1 examples") {
    assert(minimumOrganizeEnergy(parseState(exampleInput)) == 12521)
  }

  ignore("Part 1 input answer") {
    assert(minimumOrganizeEnergy(parseState(input)) == 15237)
  }

  // TODO: optimize
  ignore("Part 2 examples") {
    assert(minimumOrganizeEnergy(parseState(exampleInput)) == 44169)
  }

  ignore("Part 2 input answer") {
    assert(minimumOrganizeEnergy(parseState(input)) == 47509)
  }
}
