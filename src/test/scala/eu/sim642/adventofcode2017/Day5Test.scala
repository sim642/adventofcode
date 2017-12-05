package eu.sim642.adventofcode2017

import Day5._
import org.scalatest.FunSuite

class Day5Test extends FunSuite {

  test("Part 1 examples") {
    assert(OffsetState(Seq(0, 3, 0, 1, -3), 0).jump == OffsetState(Seq(1, 3, 0, 1, -3), 0))
    assert(OffsetState(Seq(1, 3, 0, 1, -3), 0).jump == OffsetState(Seq(2, 3, 0, 1, -3), 1))
    assert(OffsetState(Seq(2, 3, 0, 1, -3), 1).jump == OffsetState(Seq(2, 4, 0, 1, -3), 4))
    assert(OffsetState(Seq(2, 4, 0, 1, -3), 4).jump == OffsetState(Seq(2, 4, 0, 1, -2), 1))
    assert(OffsetState(Seq(2, 4, 0, 1, -2), 1).jump == OffsetState(Seq(2, 5, 0, 1, -2), 5))

    assert(exitSteps("0\n3\n0\n1\n-3") == 5)
  }

  test("Part 1 input answer") {
    assert(exitSteps(input) == 376976)
  }
}
