package eu.sim642.adventofcode2023

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  val exampleInput =
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin

  val exampleInput2 =
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin

  test("Part 1 examples") {
    assert(Part1.sumCalibrationValues(parseDocument(exampleInput)) == 142)
  }

  test("Part 1 input answer") {
    assert(Part1.sumCalibrationValues(parseDocument(input)) == 55002)
  }

  test("Part 2 examples") {
    assert(Part2.sumCalibrationValues(parseDocument(exampleInput2)) == 281)
  }

  test("Part 2 input answer") {
    assert(Part2.sumCalibrationValues(parseDocument(input)) == 55093)
  }
}
