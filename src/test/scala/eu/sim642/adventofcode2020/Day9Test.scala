package eu.sim642.adventofcode2020

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  val exampleInput =
    """35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576""".stripMargin

  test("Part 1 examples") {
    assert(firstInvalid(parseNumbers(exampleInput), 5) == 127)
  }

  test("Part 1 input answer") {
    assert(firstInvalid(parseNumbers(input)) == 248131121)
  }

  test("Part 2 examples") {
    assert(encryptionWeakness(parseNumbers(exampleInput), 5) == 62)
  }

  test("Part 2 input answer") {
    assert(encryptionWeakness(parseNumbers(input)) == 31580383)
  }
}
