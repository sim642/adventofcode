package eu.sim642.adventofcode2018

import Day5._
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

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

  test("Part 2 examples for part 1") {
    assert(reactPolymer("dbcCCBcCcD") == "dbCBcD")
    assert(reactPolymer("daAcCaCAcCcaDA") == "daCAcaDA")
    assert(reactPolymer("dabAaBAaDA") == "daDA")
    assert(reactPolymer("abAcCaCBAcCcaA") == "abCBAc")
  }

  test("Part 2 examples") {
    assert(bestPolymerLength("dabAcCaCBAcCcaDA") == 4)
  }

  test("Part 2 input answer") {
    assert(bestPolymerLength(input) == 5330)
  }
}
