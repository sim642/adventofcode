package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day8._

class Day8Test extends FunSuite {

  val exampleInput = "123456789012"

  test("parseLayers") {
    assert(parseLayers(exampleInput, 3, 2) ==
      Seq(
        Vector(
          Vector(1, 2, 3),
          Vector(4, 5, 6),
        ),
        Vector(
          Vector(7, 8, 9),
          Vector(0, 1, 2),
        ),
      )
    )
  }

  test("Part 1 input answer") {
    assert(fewestZeroLayer(parseLayers(input)) == 2975)
  }
}
