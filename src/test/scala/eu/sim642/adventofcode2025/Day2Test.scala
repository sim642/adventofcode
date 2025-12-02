package eu.sim642.adventofcode2025

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  val exampleInput =
    """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"""

  test("Part 1 examples") {
    assert(sumInvalidIds(parseRanges(exampleInput)) == 1227775554)
  }

  test("Part 1 input answer") {
    assert(sumInvalidIds(parseRanges(input)) == 5398419778L)
  }
}
