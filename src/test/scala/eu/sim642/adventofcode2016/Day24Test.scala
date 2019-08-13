package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day24._
import eu.sim642.adventofcodelib.pos.Pos

class Day24Test extends FunSuite {

  val exampleInput =
    """###########
      |#0.1.....2#
      |#.#######.#
      |#4.......3#
      |###########""".stripMargin

  test("findPois") {
    assert(findPois(parseGrid(exampleInput)) == Map(
      0 -> Pos(1, 1),
      1 -> Pos(3, 1),
      2 -> Pos(9, 1),
      3 -> Pos(9, 3),
      4 -> Pos(1, 3),
    ))
  }

  test("Part 1 examples") {
    assert(shortestRoute(exampleInput) == 14)
  }

  test("Part 1 input answer") {
    assert(shortestRoute(input) == 470)
  }

  test("Part 2 input answer") {
    assert(shortestRouteReturn(input) == 720)
  }
}
