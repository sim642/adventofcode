package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day5._

class Day5Test extends FunSuite {

  test("Part 1 examples") {
    assert(isNice("ugknbfddgicrmopn"))
    assert(isNice("aaa"))
    assert(!isNice("jchzalrnumimnmhp"))
    assert(!isNice("haegwjzuvuyypxyu"))
    assert(!isNice("dvszwmarrgswjxmb"))
  }

  test("Part 1 input answer") {
    assert(countNice(input) == 238)
  }
}
