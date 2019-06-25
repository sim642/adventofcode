package eu.sim642.adventofcode2016

import Day11._
import org.scalatest.FunSuite

class Day11Test extends FunSuite {

  val exampleInput =
    """The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
      |The second floor contains a hydrogen generator.
      |The third floor contains a lithium generator.
      |The fourth floor contains nothing relevant.""".stripMargin

  val exampleInitialState =
    State(
      Vector(
        Set(Microchip("hydrogen"), Microchip("lithium")),
        Set(Generator("hydrogen")),
        Set(Generator("lithium")),
        Set()
      ),
      0
    )

  test("parseInput") {
    assert(parseInput(exampleInput) == exampleInitialState)
  }

  test("Part 1 examples") {
    assert(solveSteps(exampleInitialState) == 11)
  }

  test("Part 1 input answer") {
    assert(solveSteps(input) == 33)
  }

  // needs optimization

  ignore("Part 2 input answer") {
    assert(solveStepsExtra(input) == 57) // 410 seconds
  }
}
