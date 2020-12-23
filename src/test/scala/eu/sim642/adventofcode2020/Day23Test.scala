package eu.sim642.adventofcode2020

import Day23._
import org.scalatest.funsuite.AnyFunSuite

class Day23Test extends AnyFunSuite {

  val exampleInput = "389125467"

  test("Part 1 examples") {
    assert(simulateMove(parseCups(exampleInput)) == parseCups("289154673"))

    assert(simulateMovesLabels(parseCups(exampleInput), 10) == "92658374")
    assert(simulateMovesLabels(parseCups(exampleInput)) == "67384529")
  }

  test("Part 1 input answer") {
    assert(simulateMovesLabels(parseCups(input)) == "52864379")
  }
}
