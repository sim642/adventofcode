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
    assert(errorCorrect(exampleInput) == "easter")
  }

  test("Part 1 input answer") {
    assert(errorCorrect(input) == "umcvzsmw")
  }
}
