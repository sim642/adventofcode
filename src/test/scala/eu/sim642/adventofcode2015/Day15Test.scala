package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day15._

class Day15Test extends FunSuite {

  val exampleInput =
    """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
      |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3""".stripMargin

  test("Part 1 examples") {
    assert(highestScore(exampleInput) == 62842880)
  }

  test("Part 1 input answer") {
    assert(highestScore(input) == 222870)
  }

  test("Part 2 examples") {
    assert(highestCalorieScore(exampleInput) == 57600000)
  }

  test("Part 2 input answer") {
    assert(highestCalorieScore(input) == 117936)
  }
}
