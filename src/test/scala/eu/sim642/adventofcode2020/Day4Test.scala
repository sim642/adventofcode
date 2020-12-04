package eu.sim642.adventofcode2020

import Day4._
import org.scalatest.funsuite.AnyFunSuite

class Day4Test extends AnyFunSuite {

  val exampleInput =
    """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
      |byr:1937 iyr:2017 cid:147 hgt:183cm
      |
      |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
      |hcl:#cfa07d byr:1929
      |
      |hcl:#ae17e1 iyr:2013
      |eyr:2024
      |ecl:brn pid:760753108 byr:1931
      |hgt:179cm
      |
      |hcl:#cfa07d eyr:2025 pid:166559648
      |iyr:2011 ecl:brn hgt:59in""".stripMargin

  test("Part 1 examples") {
    assert(countValid(parsePassports(exampleInput)) == 2)
  }

  test("Part 1 input answer") {
    assert(countValid(parsePassports(input)) == 226)
  }
}
