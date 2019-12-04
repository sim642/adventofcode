package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day4._

class Day4Test extends FunSuite {

  test("Part 1 examples") {
    assert(Part1.isPassword(111111))
    assert(!Part1.isPassword(223450))
    assert(!Part1.isPassword(123789))
  }

  test("Part 1 input answer") {
    assert(Part1.countPasswords(parseRange(input)) == 921)
  }

  test("Part 2 examples") {
    assert(Part2.isPassword(112233))
    assert(!Part2.isPassword(123444))
    assert(Part2.isPassword(111122))

    assert(Part2.isPassword(112222))
  }

  test("Part 2 input answer") {
    assert(Part2.countPasswords(parseRange(input)) == 603)
  }
}
