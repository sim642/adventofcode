package eu.sim642.adventofcode2023

import Day22._
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends AnyFunSuite {

  private val exampleInput =
    """1,0,1~1,2,1
      |0,0,2~2,0,2
      |0,2,3~2,2,3
      |0,0,4~0,2,4
      |2,0,5~2,2,5
      |0,1,6~2,1,6
      |1,1,8~1,1,9""".stripMargin

  test("Part 1 examples") {
    assert(countDisintegrable(parseBricks(exampleInput)) == 5)
  }

  test("Part 1 input answer") {
    assert(countDisintegrable(parseBricks(input)) == 428)
  }

  test("Part 2 examples") {
    assert(sumDisintegrateFall(parseBricks(exampleInput)) == 7)
  }

  test("Part 2 fall into same place") {
    assert(sumDisintegrateFall(parseBricks(
      """0,0,1~0,0,1
        |0,0,2~0,0,2""".stripMargin
    )) == 1)
  }

  test("Part 2 input answer") {
    assert(sumDisintegrateFall(parseBricks(input)) == 35654)
  }
}
