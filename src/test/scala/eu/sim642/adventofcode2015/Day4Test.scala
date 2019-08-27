package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day4._

class Day4Test extends FunSuite {

  test("Part 1 examples") {
    assert(findZeroHash("abcdef") == 609043)
    assert(findZeroHash("pqrstuv") == 1048970)
  }

  test("Part 1 input answer") {
    assert(findZeroHash(input) == 117946)
  }
}
