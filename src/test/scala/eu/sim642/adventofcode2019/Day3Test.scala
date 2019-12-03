package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day3._
import eu.sim642.adventofcodelib.pos.Pos

class Day3Test extends FunSuite {

  val exampleInput1 =
    """R8,U5,L5,D3
      |U7,R6,D4,L4""".stripMargin

  val exampleInput2 =
    """R75,D30,R83,U83,L12,D49,R71,U7,L72
      |U62,R66,U55,R34,D71,R55,D58,R83""".stripMargin

  val exampleInput3 =
    """R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
      |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7""".stripMargin

  test("parsePath") {
    assert(parsePath("R8,U5,L5,D3") ==
      Seq(
        (Pos(1, 0), 8),
        (Pos(0, 1), 5),
        (Pos(-1, 0), 5),
        (Pos(0, -1), 3),
      ))
  }

  test("Part 1 examples") {
    assert(findCentralIntersectionDistance(parseInput(exampleInput1)) == 6)
    assert(findCentralIntersectionDistance(parseInput(exampleInput2)) == 159)
    assert(findCentralIntersectionDistance(parseInput(exampleInput3)) == 135)
  }

  test("Part 1 input answer") {
    assert(findCentralIntersectionDistance(parseInput(input)) == 1626)
  }

  test("Part 2 examples") {
    assert(findClosestIntersectionDistance(parseInput(exampleInput1)) == 30)
    assert(findClosestIntersectionDistance(parseInput(exampleInput2)) == 610)
    assert(findClosestIntersectionDistance(parseInput(exampleInput3)) == 410)
  }

  test("Part 2 input answer") {
    assert(findClosestIntersectionDistance(parseInput(input)) == 27330)
  }
}
