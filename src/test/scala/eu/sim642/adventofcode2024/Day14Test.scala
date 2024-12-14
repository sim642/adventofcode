package eu.sim642.adventofcode2024

import Day14.*
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  val exampleInput =
    """p=0,4 v=3,-3
      |p=6,3 v=-1,-3
      |p=10,3 v=-1,2
      |p=2,0 v=2,-1
      |p=0,0 v=1,3
      |p=3,0 v=-2,-2
      |p=7,6 v=-1,-3
      |p=3,0 v=-1,-2
      |p=9,3 v=2,3
      |p=7,3 v=-1,2
      |p=2,4 v=2,-3
      |p=9,5 v=-3,-3""".stripMargin

  test("Part 1 examples") {
    assert(safetyFactor(parseRobots(exampleInput), roomSize = Pos(11, 7)) == 12)
  }

  test("Part 1 input answer") {
    assert(safetyFactor(parseRobots(input)) == 228690000)
  }

  test("Part 2 input answer") {
    assert(findEasterEgg(parseRobots(input)) == 7093)
  }
}
