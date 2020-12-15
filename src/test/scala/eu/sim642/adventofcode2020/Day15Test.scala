package eu.sim642.adventofcode2020

import Day15._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day15Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    val inputExpectedNumber = Table(
      ("input", "expectedNumber"),
      ("0,3,6", 436),
      ("1,3,2", 1),
      ("2,1,3", 10),
      ("1,2,3", 27),
      ("2,3,1", 78),
      ("3,2,1", 438),
      ("3,1,2", 1836),
    )

    forAll(inputExpectedNumber) { (input, expectedNumber) =>
      assert(simulateNumber(parseStartingNumbers(input)) == expectedNumber)
    }
  }

  test("Part 1 input answer") {
    assert(simulateNumber(parseStartingNumbers(input)) == 981)
  }

  // TODO: optimize (2m 22s)
  ignore("Part 2 examples") {
    val inputExpectedNumber = Table(
      ("input", "expectedNumber"),
      ("0,3,6", 175594),
      ("1,3,2", 2578),
      ("2,1,3", 3544142),
      ("1,2,3", 261214),
      ("2,3,1", 6895259),
      ("3,2,1", 18),
      ("3,1,2", 362),
    )

    forAll(inputExpectedNumber) { (input, expectedNumber) =>
      assert(simulateNumber(parseStartingNumbers(input), part2ReturnIndex) == expectedNumber)
    }
  }

  // TODO: optimize (19s)
  ignore("Part 2 input answer") {
    assert(simulateNumber(parseStartingNumbers(input), part2ReturnIndex) == 164878)
  }
}
