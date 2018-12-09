package eu.sim642.adventofcode2018

import eu.sim642.AdventOfCodeSuite
import eu.sim642.adventofcode2018.Day3._
import org.scalatest.FunSuite

class Day3Test extends FunSuite with AdventOfCodeSuite {

  val exampleInput = """#1 @ 1,3: 4x4
                       |#2 @ 3,1: 4x4
                       |#3 @ 5,5: 2x2""".stripMargin

  test("parseRectangle") {
    assert(parseRectangle("#123 @ 3,2: 5x4") == Rectangle(123, 3, 2, 5, 4))
  }

  test("Part 1 examples") {
    assert(overlappingFabric(parseRectangles(exampleInput)) == 4)
  }

  test("Part 1 input answer") {
    assert(overlappingFabric(parseRectangles(input)) == 115242)
  }

  test("Part 2 examples") {
    assert(nonOverlappingRectangle(parseRectangles(exampleInput)) == 3)
  }

  test("Part 2 input answer") {
    assert(nonOverlappingRectangle(parseRectangles(input)) == 1046)
  }
}
