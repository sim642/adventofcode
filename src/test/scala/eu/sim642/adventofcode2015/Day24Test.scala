package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day24._

class Day24Test extends FunSuite {

  val exampleWeights = ((1 to 5) ++ (7 to 11)).toList

  test("Part 1 examples") {
    assert(idealFirstQE(exampleWeights) == 99)
  }

  test("Part 1 input answer") {
    assert(idealFirstQE(input) == 11846773891L)
  }
}
