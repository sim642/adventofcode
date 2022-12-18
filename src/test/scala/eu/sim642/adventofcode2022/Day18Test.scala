package eu.sim642.adventofcode2022

import Day18._
import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite {

  val exampleInput =
    """2,2,2
      |1,2,2
      |3,2,2
      |2,1,2
      |2,3,2
      |2,2,1
      |2,2,3
      |2,2,4
      |2,2,6
      |1,2,5
      |3,2,5
      |2,1,5
      |2,3,5""".stripMargin

  test("Part 1 examples") {
    assert(surfaceArea(parseDroplets(exampleInput)) == 64)
  }

  test("Part 1 input answer") {
    assert(surfaceArea(parseDroplets(input)) == 4302)
  }

  test("Part 2 examples") {
    assert(exteriorSurfaceArea(parseDroplets(exampleInput)) == 58)
  }

  test("Part 2 input answer") {
    assert(exteriorSurfaceArea(parseDroplets(input)) == 2492)
  }
}
