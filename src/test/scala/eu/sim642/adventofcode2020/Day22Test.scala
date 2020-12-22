package eu.sim642.adventofcode2020

import Day22._
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends AnyFunSuite {

  val exampleInput =
    """Player 1:
      |9
      |2
      |6
      |3
      |1
      |
      |Player 2:
      |5
      |8
      |4
      |7
      |10""".stripMargin

  test("Part 1 examples") {
    assert(winningScore(parseDecks(exampleInput)) == 306)
  }

  test("Part 1 input answer") {
    assert(winningScore(parseDecks(input)) == 35202)
  }
}
