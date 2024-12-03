package eu.sim642.adventofcode2024

import Day3._
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {

  val exampleInput = """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
  val exampleInput2 = """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"""

  test("Part 1 examples") {
    assert(sumUncorruptedMuls(exampleInput) == 161)
  }

  test("Part 1 input answer") {
    assert(sumUncorruptedMuls(input) == 182780583)
  }

  test("Part 2 examples") {
    assert(sumUncorruptedMuls2(exampleInput2) == 48)
  }

  test("Part 2 input answer") {
    assert(sumUncorruptedMuls2(input) == 90772405)
  }
}
