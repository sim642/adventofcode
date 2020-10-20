package eu.sim642.adventofcode2018

import Day14._
import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(tenScores(9) == "5158916779")
    assert(tenScores(5) == "0124515891")
    assert(tenScores(18) == "9251071085")
    assert(tenScores(2018) == "5941429882")
  }

  test("Part 1 input answer") {
    assert(tenScores(input) == "5482326119")
  }

  test("Part 2 examples") {
    assert(recipesToLeft("51589") == 9)
    assert(recipesToLeft("01245") == 5)
    assert(recipesToLeft("92510") == 18)
    assert(recipesToLeft("59414") == 2018)
  }

  test("Part 2 input answer") {
    assert(recipesToLeft(input) == 20368140)
  }
}
