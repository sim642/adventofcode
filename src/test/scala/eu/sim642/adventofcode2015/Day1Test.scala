package eu.sim642.adventofcode2015

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(finalFloor("(())") == 0)
    assert(finalFloor("()()") == 0)
    assert(finalFloor("(((") == 3)
    assert(finalFloor("(()(()(") == 3)
    assert(finalFloor("))(((((") == 3)
    assert(finalFloor("())") == -1)
    assert(finalFloor("))(") == -1)
    assert(finalFloor(")))") == -3)
    assert(finalFloor(")())())") == -3)
  }

  test("Part 1 input answer") {
    assert(finalFloor(input) == 232)
  }

  test("Part 2 examples") {
    assert(basementPosition(")") == 1)
    assert(basementPosition("()())") == 5)
  }

  test("Part 2 input answer") {
    assert(basementPosition(input) == 1783)
  }
}
