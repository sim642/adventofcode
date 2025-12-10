package eu.sim642.adventofcode2025

import Day10._
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  val exampleInput =
    """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
      |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin

  test("Part 1 examples") {
    assert(sumFewestPresses(parseMachines(exampleInput)) == 7)
  }

  test("Part 1 input answer") {
    assert(sumFewestPresses(parseMachines(input)) == 449)
  }

  test("Part 2 examples") {
    assert(sumFewestPresses2(parseMachines(exampleInput)) == 33)
  }

  test("Part 2 input answer") {
    //assert(sumFewestPresses2(parseMachines(input)) == 449)
  }
}
