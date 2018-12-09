package eu.sim642.adventofcode2017

import eu.sim642.AdventOfCodeSuite
import eu.sim642.adventofcode2017.Day1._
import org.scalatest.FunSuite

class Day1Test extends FunSuite with AdventOfCodeSuite {

  test("Part 1 examples") {
    assert(Part1.captcha("1122") == 3)
    assert(Part1.captcha("1111") == 4)
    assert(Part1.captcha("1234") == 0)
    assert(Part1.captcha("91212129") == 9)
  }

  test("Part 1 input answer") {
    assert(Part1.captcha(input) == 1069)
  }

  test("Part 2 examples") {
    assert(Part2.captcha("1212") == 6)
    assert(Part2.captcha("1221") == 0)
    assert(Part2.captcha("123425") == 4)
    assert(Part2.captcha("123123") == 12)
    assert(Part2.captcha("12131415") == 4)
  }

  test("Part 2 input answer") {
    assert(Part2.captcha(input) == 1268)
  }

}
