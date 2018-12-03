package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2018.Day3._
import org.scalatest.FunSuite

class Day3Test extends FunSuite {

  test("parseRectangle") {
    assert(parseRectangle("#123 @ 3,2: 5x4") == Rectangle(123, 3, 2, 5, 4))
  }

  test("Part 1 examples") {
    assert(overlappingFabric(parseRectangles(
      """#1 @ 1,3: 4x4
        |#2 @ 3,1: 4x4
        |#3 @ 5,5: 2x2""".stripMargin
    )) == 4)
  }

  test("Part 1 input answer") {
    assert(overlappingFabric(parseRectangles(input)) == 115242)
  }
}
