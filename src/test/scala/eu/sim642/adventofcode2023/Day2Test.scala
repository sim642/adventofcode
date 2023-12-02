package eu.sim642.adventofcode2023

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  private val exampleInput =
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin

  test("Part 1 examples") {
    assert(sumPossibleIds(parseGames(exampleInput)) == 8)
  }

  test("Part 1 input answer") {
    assert(sumPossibleIds(parseGames(input)) == 2720)
  }

  test("Part 2 examples") {
    assert(sumPowers(parseGames(exampleInput)) == 2286)
  }

  test("Part 2 input answer") {
    assert(sumPowers(parseGames(input)) == 71535)
  }
}
