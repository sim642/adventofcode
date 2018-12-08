package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day8._

class Day8Test extends FunSuite {

  val exampleInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

  test("Part 1 examples") {
    assert(metadataSum(parseTree(exampleInput)) == 138)
  }

  test("Part 1 input answer") {
    assert(metadataSum(parseTree(input)) == 46578)
  }
}
