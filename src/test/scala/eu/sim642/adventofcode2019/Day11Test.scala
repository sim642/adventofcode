package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day9.parseProgram
import Day11._

class Day11Test extends FunSuite {

  test("Part 1 examples") {
    val program = parseProgram("104,1,104,0,104,0,104,0,104,1,104,0,104,1,104,0,104,0,104,1,104,1,104,0,104,1,104,0,99")
    assert(countPainted(program) == 6)
  }

  test("Part 1 input answer") {
    assert(countPainted(parseProgram(input)) == 1863)
  }
}
