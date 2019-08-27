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

  test("Part 2 examples") {
    assert(ribbonLength(2, 3, 4) == 34)
    assert(ribbonLength(1, 1, 10) == 14)

    assert(totalRibbonLength("2x3x4") == 34)
    assert(totalRibbonLength("1x1x10") == 14)
  }

  test("Part 2 input answer") {
    assert(totalRibbonLength(input) == 3783758)
  }
}
