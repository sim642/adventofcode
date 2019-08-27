package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day6._

class Day6Test extends FunSuite {

  test("Part 1 input answer") {
    assert(countLit(input) == 569999)
  }

  test("Part 2 examples") {
    assert(totalBrightness("turn on 0,0 through 0,0") == 1)
    assert(totalBrightness("toggle 0,0 through 999,999") == 2000000)
  }

  test("Part 2 input answer") {
    assert(totalBrightness(input) == 17836115)
  }
}
