package eu.sim642.adventofcode2018

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(resultingFreq(Seq(+1, -2, +3, +1)) == 3)
    assert(resultingFreq(Seq(+1, +1, +1)) == 3)
    assert(resultingFreq(Seq(+1, +1, -2)) == 0)
    assert(resultingFreq(Seq(-1, -2, -3)) == -6)
  }

  test("Part 1 input answer") {
    assert(resultingFreq(inputFreqChanges) == 502)
  }

  test("Part 2 examples") {
    assert(firstTwiceFreq(Seq(+1, -2, +3, +1)) == 2)
    assert(firstTwiceFreq(Seq(+1, -1)) == 0)
    assert(firstTwiceFreq(Seq(+3, +3, +4, -2, -4)) == 10)
    assert(firstTwiceFreq(Seq(-6, +3, +8, +5, -6)) == 5)
    assert(firstTwiceFreq(Seq(+7, +7, -2, -7, -4)) == 14)
  }

  test("Part 2 input answer") {
    assert(firstTwiceFreq(inputFreqChanges) == 71961)
  }
}
