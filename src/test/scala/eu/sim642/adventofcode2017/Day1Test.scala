package eu.sim642.adventofcode2017

import org.scalatest.FunSuite

class Day1Test extends FunSuite {

  test("Part 1") {
    import eu.sim642.adventofcode2017.Day1.Part1.captcha

    assert(captcha("1122") == 3)
    assert(captcha("1111") == 4)
    assert(captcha("1234") == 0)
    assert(captcha("91212129") == 9)
  }

  test("Part 2") {
    import eu.sim642.adventofcode2017.Day1.Part2.captcha

    assert(captcha("1212") == 6)
    assert(captcha("1221") == 0)
    assert(captcha("123425") == 4)
    assert(captcha("123123") == 12)
    assert(captcha("12131415") == 4)
  }

}
