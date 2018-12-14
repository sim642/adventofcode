package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day14._

class Day14Test extends FunSuite {

  test("Part 1 examples") {
    assert(tenScores(9) == "5158916779")
    assert(tenScores(5) == "0124515891")
    assert(tenScores(18) == "9251071085")
    assert(tenScores(2018) == "5941429882")
  }

  test("Part 1 input answer") {
    assert(tenScores(input) == "5482326119")
  }
}
