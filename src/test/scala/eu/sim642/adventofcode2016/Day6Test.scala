package eu.sim642.adventofcode2016

import Day6._
import org.scalatest.FunSuite

class Day6Test extends FunSuite {

  val exampleInput =
    """eedadn
      |drvtee
      |eandsr
      |raavrd
      |atevrs
      |tsrnev
      |sdttsa
      |rasrtv
      |nssdts
      |ntnada
      |svetve
      |tesnvt
      |vntsnd
      |vrdear
      |dvrsen
      |enarar""".stripMargin

  test("Part 1 examples") {
    assert(Part1.errorCorrect(exampleInput) == "easter")
  }

  test("Part 1 input answer") {
    assert(Part1.errorCorrect(input) == "umcvzsmw")
  }

  test("Part 2 examples") {
    assert(Part2.errorCorrect(exampleInput) == "advent")
  }

  test("Part 2 input answer") {
    assert(Part2.errorCorrect(input) == "rwqoacfz")
  }
}
