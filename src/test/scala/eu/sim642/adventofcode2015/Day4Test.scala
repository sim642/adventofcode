package eu.sim642.adventofcode2015

import Day4._
import org.scalatest.funsuite.AnyFunSuite

class Day4Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(Part1.findZeroHash("abcdef") == 609043)
    assert(Part1.findZeroHash("pqrstuv") == 1048970)
  }

  test("Part 1 input answer") {
    assert(Part1.findZeroHash(input) == 117946)
  }

  test("Part 2 input answer") {
    assert(Part2.findZeroHash(input) == 3938038)
  }
}
