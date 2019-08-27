package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day3._

class Day3Test extends FunSuite {

  test("Part 1 examples") {
    assert(Part1.countAtLeastOne(">") == 2)
    assert(Part1.countAtLeastOne("^>v<") == 4)
    assert(Part1.countAtLeastOne("^v^v^v^v^v") == 2)
  }

  test("Part 1 input answer") {
    assert(Part1.countAtLeastOne(input) == 2081)
  }

  test("Part 2 examples") {
    assert(Part2.countAtLeastOne("^v") == 3)
    assert(Part2.countAtLeastOne("^>v<") == 3)
    assert(Part2.countAtLeastOne("^v^v^v^v^v") == 11)
  }

  test("Part 2 input answer") {
    assert(Part2.countAtLeastOne(input) == 2341)
  }
}
