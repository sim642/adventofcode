package eu.sim642.adventofcode2017

import eu.sim642.adventofcode2017.Day1.captcha
import org.scalatest.FunSuite

class Day1Test extends FunSuite {

  test("captcha") {
    assert(captcha("1122") == 3)
    assert(captcha("1111") == 4)
    assert(captcha("1234") == 0)
    assert(captcha("91212129") == 9)
  }

}
