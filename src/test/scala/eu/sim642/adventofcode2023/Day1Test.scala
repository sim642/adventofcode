package eu.sim642.adventofcode2023

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  val exampleInput =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin

  test("Part 1 examples") {
    assert(sumCalibrationValues(parseDocument(exampleInput)) == 142)
  }

  test("Part 1 input answer") {
    assert(sumCalibrationValues(parseDocument(input)) == 55002)
  }
}
