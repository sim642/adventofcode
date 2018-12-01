package eu.sim642.adventofcode2018

import Day1._
import org.scalatest.FunSuite

class Day1Test extends FunSuite {

  test("Part 1 examples") {
    assert(resultingFreq(Seq(+1, -2, +3, +1)) == 3)
    assert(resultingFreq(Seq(+1, +1, +1)) == 3)
    assert(resultingFreq(Seq(+1, +1, -2)) == 0)
    assert(resultingFreq(Seq(-1, -2, -3)) == -6)
  }

  test("Part 1 input answer") {
    assert(resultingFreq(inputFreqChanges) == 502)
  }
}
