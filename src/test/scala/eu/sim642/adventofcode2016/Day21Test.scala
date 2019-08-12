package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day21._

class Day21Test extends FunSuite {

  val exampleInput =
    """swap position 4 with position 0
      |swap letter d with letter b
      |reverse positions 0 through 4
      |rotate left 1 step
      |move position 1 to position 4
      |move position 3 to position 0
      |rotate based on position of letter b
      |rotate based on position of letter d""".stripMargin

  test("parseInput") {
    assert(parseInput(exampleInput) == Seq(
      SwapPosition(4, 0),
      SwapLetter('d', 'b'),
      Reverse(0, 4),
      RotateSteps(1),
      Move(1, 4),
      Move(3, 0),
      RotateLetter('b'),
      RotateLetter('d'),
    ))
  }

  test("Part 1 examples") {
    assert(scramble(exampleInput, "abcde") == "decab")
  }

  test("Part 1 input answer") {
    assert(scramble(input) == "aefgbcdh")
  }

  test("Part 2 input answer") {
    assert(unscramble(input) == "egcdahbf")
  }
}
