package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day12._

class Day12Test extends FunSuite {

  test("Part 1 examples") {
    assert(sumNumbers("""[1,2,3]""") == 6)
    assert(sumNumbers("""{"a":2,"b":4}""") == 6)
    assert(sumNumbers("""[[[3]]]""") == 3)
    assert(sumNumbers("""{"a":{"b":4},"c":-1}""") == 3)
    assert(sumNumbers("""{"a":[-1,1]}""") == 0)
    assert(sumNumbers("""[-1,{"a":1}]""") == 0)
    assert(sumNumbers("""[]""") == 0)
    assert(sumNumbers("""{}""") == 0)
  }

  test("Part 1 input answer") {
    assert(sumNumbers(input) == 119433)
  }
}
