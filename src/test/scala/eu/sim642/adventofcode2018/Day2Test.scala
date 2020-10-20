package eu.sim642.adventofcode2018

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

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

  test("Part 2 examples") {
    assert(commonCorrectIds(
      """abcde
        |fghij
        |klmno
        |pqrst
        |fguij
        |axcye
        |wvxyz
      """.stripMargin.linesIterator.toSeq) == "fgij")
  }

  test("Part 2 input answer") {
    assert(commonCorrectIds(inputLines) == "lufjygedpvfbhftxiwnaorzmq")
  }
}
