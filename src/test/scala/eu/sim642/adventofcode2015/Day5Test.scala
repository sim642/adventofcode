package eu.sim642.adventofcode2015

import Day5._
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(Part1.isNice("ugknbfddgicrmopn"))
    assert(Part1.isNice("aaa"))
    assert(!Part1.isNice("jchzalrnumimnmhp"))
    assert(!Part1.isNice("haegwjzuvuyypxyu"))
    assert(!Part1.isNice("dvszwmarrgswjxmb"))
  }

  test("Part 1 input answer") {
    assert(Part1.countNice(input) == 238)
  }

  test("Part 2 examples") {
    assert(Part2.isNice("qjhvhtzxzqqjkmpb"))
    assert(Part2.isNice("xxyxx"))
    assert(!Part2.isNice("uurcxstgmygtbstg"))
    assert(!Part2.isNice("ieodomkazucvgmuy"))
  }

  test("Part 2 input answer") {
    assert(Part2.countNice(input) == 69)
  }
}
