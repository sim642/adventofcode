package eu.sim642.adventofcode2018

import Day25._
import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite {

  val exampleInput1 =
    """0,0,0,0
      |3,0,0,0
      |0,3,0,0
      |0,0,3,0
      |0,0,0,3
      |0,0,0,6
      |9,0,0,0
      |12,0,0,0""".stripMargin

  val exampleInput2 =
    """-1,2,2,0
      |0,0,2,-2
      |0,0,0,-2
      |-1,2,0,0
      |-2,-2,-2,2
      |3,0,2,-1
      |-1,3,2,2
      |-1,0,-1,0
      |0,2,1,-2
      |3,0,0,0""".stripMargin

  val exampleInput3 =
    """1,-1,0,1
      |2,0,-1,0
      |3,2,-1,0
      |0,0,3,1
      |0,0,-1,-1
      |2,3,-2,0
      |-2,2,0,0
      |2,-2,0,-1
      |1,-1,0,-1
      |3,2,0,2""".stripMargin

  val exampleInput4 =
    """1,-1,-1,-2
      |-2,-2,0,1
      |0,2,1,3
      |-2,3,-2,1
      |0,2,3,-2
      |-1,-1,1,-2
      |0,-2,-1,0
      |-2,2,3,-1
      |1,2,2,0
      |-1,-2,0,-2""".stripMargin

  test("Part 1 examples") {
    assert(countConstellations(parseInput(exampleInput1)) == 2)
    assert(countConstellations(parseInput(exampleInput2)) == 4)
    assert(countConstellations(parseInput(exampleInput3)) == 3)
    assert(countConstellations(parseInput(exampleInput4)) == 8)
  }

  test("Part 1 input answer") {
    assert(countConstellations(parseInput(input)) == 327)
  }
}
