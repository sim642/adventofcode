package eu.sim642.adventofcode2017

import Day3._
import org.scalatest.FunSuite

class Day3Test extends FunSuite {

  test("coord") {
    assert(coord(1) == (0, 0))

    assert(coord(2) == (1, 0))
    assert(coord(3) == (1, 1))
    assert(coord(4) == (0, 1))
    assert(coord(5) == (-1, 1))
    assert(coord(6) == (-1, 0))
    assert(coord(7) == (-1, -1))
    assert(coord(8) == (0, -1))
    assert(coord(9) == (1, -1))

    assert(coord(10) == (2, -1))
    assert(coord(11) == (2, 0))
    assert(coord(12) == (2, 1))
    assert(coord(13) == (2, 2))
    assert(coord(14) == (1, 2))
    assert(coord(15) == (0, 2))
    assert(coord(16) == (-1, 2))
    assert(coord(17) == (-2, 2))
    assert(coord(18) == (-2, 1))
    assert(coord(19) == (-2, 0))
    assert(coord(20) == (-2, -1))
    assert(coord(21) == (-2, -2))
    assert(coord(22) == (-1, -2))
    assert(coord(23) == (0, -2))
    assert(coord(24) == (1, -2))
    assert(coord(25) == (2, -2))
  }

  test("Part 1 example") {
    assert(distance(1) == 0)
    assert(distance(12) == 3)
    assert(distance(23) == 2)
    assert(distance(1024) == 31)
  }

  test("Part 1 input answer") {
    assert(distance(input) == 419)
  }
}
