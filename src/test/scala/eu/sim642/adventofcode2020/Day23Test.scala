package eu.sim642.adventofcode2020

import Day23._
import org.scalatest.funsuite.AnyFunSuite

class Day23Test extends AnyFunSuite {

  val exampleInput = "389125467"

  test("Part 1 examples") {
    assert(simulateMovesLabels(parseCups(exampleInput), 10) == "92658374")
    assert(simulateMovesLabels(parseCups(exampleInput)) == "67384529")
  }

  test("Part 1 input answer") {
    assert(simulateMovesLabels(parseCups(input)) == "52864379")
  }

  test("Part 2 examples") {
    assert(simulateMovesLabelsPart2(parseCups(exampleInput)) == 149245887792L)
  }

  test("Part 2 input answer") {
    assert(simulateMovesLabelsPart2(parseCups(input)) == 11591415792L)
  }
}
