package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day9.parseProgram
import Day11._

class Day11Test extends FunSuite {

  // manually constructed Intcode program which just outputs the example values
  val exampleInput = "104,1,104,0,104,0,104,0,104,1,104,0,104,1,104,0,104,0,104,1,104,1,104,0,104,1,104,0,99"

  test("Part 1 examples") {
    assert(countPainted(parseProgram(exampleInput)) == 6)
  }

  test("Part 1 input answer") {
    assert(countPainted(parseProgram(input)) == 1863)
  }

  test("Part 2 input answer") {
    renderIdentifier(parseProgram(input)) // BLULZJLZ
  }
}
