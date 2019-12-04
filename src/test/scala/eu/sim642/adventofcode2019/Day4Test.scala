package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day4._

class Day4Test extends FunSuite {

  test("Part 1 examples") {
    assert(isPassword(111111))
    assert(!isPassword(223450))
    assert(!isPassword(123789))
  }

  test("Part 1 input answer") {
    assert(countPasswords(parseRange(input)) == 921)
  }
}
