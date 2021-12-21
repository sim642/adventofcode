package eu.sim642.adventofcode2021

import Day21._
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {

  val exampleInput =
    """Player 1 starting position: 4
      |Player 2 starting position: 8""".stripMargin

  test("parsePlayers") {
    assert(parsePlayers(exampleInput) == (Player(4), Player(8)))
  }

  test("Part 1 examples") {
    assert(play(parsePlayers(exampleInput)) == ((Player(10, 1000), Player(3, 745)), 993))
    assert(loserScoreRolls(parsePlayers(exampleInput)) == 739785)
  }

  test("Part 1 input answer") {
    assert(loserScoreRolls(parsePlayers(input)) == 679329)
  }
}
