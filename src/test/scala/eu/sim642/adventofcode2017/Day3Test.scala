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

  test("SumIterator") {
    val it = new SumIterator
    assert(it.next() == 1)
    assert(it.next() == 1)
    assert(it.next() == 2)
    assert(it.next() == 4)
    assert(it.next() == 5)
    assert(it.next() == 10)
    assert(it.next() == 11)
    assert(it.next() == 23)
    assert(it.next() == 25)
    assert(it.next() == 26)
    assert(it.next() == 54)
    assert(it.next() == 57)
    assert(it.next() == 59)
    assert(it.next() == 122)
    // ...
  }

  test("Part 2 input answer") {
    assert(sumLarger(input) == 295229)
  }
}
