package eu.sim642.adventofcode2021

import Day17._
import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  val exampleInput = "target area: x=20..30, y=-10..-5"

  test("Part 1 examples") {
    val exampleTarget = parseTarget(exampleInput)

    assert(hitsTargetY(exampleTarget, 2))
    assert(hitsTargetY(exampleTarget, 3))
    assert(hitsTargetY(exampleTarget, 0))
    //assert(!hitsTargetY(exampleTarget, -4)) // x velocity matters for not hitting in example
    assert(hitsTargetY(exampleTarget, 9))

    assert(findHighestY(exampleTarget) == 45)
  }

  test("Part 1 input answer") {
    assert(findHighestY(parseTarget(input)) == 2775)
  }
}
