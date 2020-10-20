package eu.sim642.adventofcode2016

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  val exampleInput =
    """ULL
      |RRDDD
      |LURDL
      |UUUUD""".stripMargin

  test("Part 1 examples") {
    assert(Part1.bathroomCode(exampleInput) == "1985")
  }

  test("Part 1 input answer") {
    assert(Part1.bathroomCode(input) == "92435")
  }

  test("Part 2 examples") {
    assert(Part2.bathroomCode(exampleInput) == "5DB3")
  }

  test("Part 2 input answer") {
    assert(Part2.bathroomCode(input) == "C1A88")
  }
}
