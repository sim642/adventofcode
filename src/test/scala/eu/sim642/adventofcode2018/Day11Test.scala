package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day11._
import eu.sim642.adventofcode2017.Day3.Pos

class Day11Test extends FunSuite {

  test("powerLevel") {
    assert(powerLevel(8, Pos(3, 5)) == 4)
    assert(powerLevel(57, Pos(122, 79)) == -5)
    assert(powerLevel(39, Pos(217, 196)) == 0)
    assert(powerLevel(71, Pos(101, 153)) == 4)
  }

  test("powerLevelSquare") {
    assert(powerLevelSquare(18, Pos(33, 45)) == 29)
    assert(powerLevelSquare(42, Pos(21, 61)) == 30)
  }

  test("Part 1 examples") {
    assert(largestPowerLevelSquare(18) == Pos(33, 45))
    assert(largestPowerLevelSquare(42) == Pos(21, 61))
  }

  test("Part 1 input answer") {
    assert(largestPowerLevelSquareString(input) == "235,48")
  }
}
