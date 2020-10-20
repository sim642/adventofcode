package eu.sim642.adventofcode2018

import Day22._
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends AnyFunSuite {

  val exampleInput =
    """depth: 510
      |target: 10,10""".stripMargin

  test("parseInput") {
    assert(parseInput(exampleInput) == (510, Pos(10, 10)))
  }

  test("Part 1 examples") {
    assert(totalRiskLevel(exampleInput) == 114)
  }

  test("Part 1 input answer") {
    assert(totalRiskLevel(input) == 11359)
  }

  test("Part 2 examples") {
    assert(fastestToTarget(exampleInput) == 45)
  }

  test("Part 2 input answer") {
    assert(fastestToTarget(input) == 976)
  }
}
