package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day2._

class Day2Test extends FunSuite {

  test("Part 1 examples") {
    assert(wrappingPaperArea(2, 3, 4) == 58)
    assert(wrappingPaperArea(1, 1, 10) == 43)

    assert(totalWrappingPaperArea("2x3x4") == 58)
    assert(totalWrappingPaperArea("1x1x10") == 43)
  }

  test("Part 1 input answer") {
    assert(totalWrappingPaperArea(input) == 1588178)
  }
}
