package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day9._

class Day9Test extends FunSuite {

  test("Part 1 examples") {
    assert(highscore(9, 25) == 32)

    assert(highscore(10, 1618) == 8317)
    assert(highscore(13, 7999) == 146373)
    assert(highscore(17, 1104) == 2764)
    assert(highscore(21, 6111) == 54718)
    assert(highscore(30, 5807) == 37305)
  }

  test("Part 1 input answer") {
    assert(highscore(input) == 398371)
  }

  test("Part 2 input answer") {
    assert(highscore(input, 100) == 3212830280L)
  }
}
