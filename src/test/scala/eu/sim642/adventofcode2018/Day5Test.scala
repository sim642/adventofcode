package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day5._

class Day5Test extends FunSuite {

  test("Part 1 examples") {
    assert(reactPolymer("aA") == "")
    assert(reactPolymer("abBA") == "")
    assert(reactPolymer("abAB") == "abAB")
    assert(reactPolymer("aabAAB") == "aabAAB")

    assert(reactPolymer("dabAcCaCBAcCcaDA") == "dabCBAcaDA")
    assert(reactPolymerLength("dabAcCaCBAcCcaDA") == 10)
  }

  test("Part 1 input answer") {
    assert(reactPolymerLength(input) == 10708)
  }
}
