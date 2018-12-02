package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day2._

class Day2Test extends FunSuite {

  test("Part 1 examples") {
    assert(checksum(Seq(
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    )) == 12)
  }

  test("Part 1 input answer") {
    assert(checksum(inputLines) == 4712)
  }
}
