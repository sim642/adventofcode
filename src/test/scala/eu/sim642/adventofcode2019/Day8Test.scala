package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day8._

class Day8Test extends FunSuite {

  val exampleInput = "123456789012"
  val exampleInput2 = "0222112222120000"

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

  test("Part 2 examples") {
    assert(stackLayers(parseLayers(exampleInput2, 2, 2)) ==
      Vector(
        Vector(0, 1),
        Vector(1, 0),
      )
    )
  }

  test("Part 2 input answer") {
    val stacked = stackLayers(parseLayers(input))
    assert(stacked ==
      Vector(
        Vector(1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0),
        Vector(1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0),
        Vector(1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0),
        Vector(1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0),
        Vector(1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0),
        Vector(1, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0),
      )
    )

    printGrid(stacked) // EHRUE
  }
}
