package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day3._

class Day3Test extends FunSuite {

  test("Part 1 examples") {
    assert(countAtLeastOne(">") == 2)
    assert(countAtLeastOne("^>v<") == 4)
    assert(countAtLeastOne("^v^v^v^v^v") == 2)
  }

  test("Part 1 input answer") {
    assert(countAtLeastOne(input) == 2081)
  }
}
