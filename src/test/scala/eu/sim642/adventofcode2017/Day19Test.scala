package eu.sim642.adventofcode2017

import Day19._
import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  lazy val exampleInput: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day19example.txt")).mkString.stripLineEnd

  test("Part 1 example") {
    assert(pathLetters(exampleInput) == "ABCDEF")
  }

  test("Part 1 input answer") {
    assert(pathLetters(input) == "MOABEUCWQS")
  }

  test("Part 2 example") {
    assert(pathLength(exampleInput) == 38)
  }

  test("Part 2 input answer") {
    assert(pathLength(input) == 18058)
  }
}
