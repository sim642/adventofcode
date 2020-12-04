package eu.sim642.adventofcode2020

import Day4._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day4Test extends AnyFunSuite with ScalaCheckPropertyChecks {

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

  val exampleInputPart2Invalids =
    """eyr:1972 cid:100
      |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
      |
      |iyr:2019
      |hcl:#602927 eyr:1967 hgt:170cm
      |ecl:grn pid:012533040 byr:1946
      |
      |hcl:dab227 iyr:2012
      |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
      |
      |hgt:59cm ecl:zzz
      |eyr:2038 hcl:74454a iyr:2023
      |pid:3556412378 byr:2007""".stripMargin

  val exampleInputPart2Valids =
    """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
      |hcl:#623a2f
      |
      |eyr:2029 ecl:blu cid:129 byr:1989
      |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
      |
      |hcl:#888785
      |hgt:164cm byr:2001 iyr:2015 cid:88
      |pid:545766238 ecl:hzl
      |eyr:2022
      |
      |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719""".stripMargin

  test("Part 1 examples") {
    assert(Part1.countValid(parsePassports(exampleInput)) == 2)
  }

  test("Part 1 input answer") {
    assert(Part1.countValid(parsePassports(input)) == 226)
  }

  test("Part 2 field validators") {
    val fieldValueExpectedValid = Table(
      ("field", "value", "expectedValid"),
      ("byr", "2002", true),
      ("byr", "2003", false),

      ("hgt", "60in", true),
      ("hgt", "190cm", true),
      ("hgt", "190in", false),
      ("hgt", "190", false),

      ("hcl", "#123abc", true),
      ("hcl", "#123abz", false),
      ("hcl", "123abc", false),

      ("ecl", "brn", true),
      ("ecl", "wat", false),

      ("pid", "000000001", true),
      ("pid", "0123456789", false),
    )

    forAll(fieldValueExpectedValid) { (field, value, expectedValid) =>
      assert(Part2.fieldValidators(field)(value) == expectedValid)
    }
  }

  test("Part 2 examples") {
    assert(Part2.countValid(parsePassports(exampleInputPart2Invalids)) == 0)
    assert(Part2.countValid(parsePassports(exampleInputPart2Valids)) == 4)
  }

  test("Part 2 input answer") {
    assert(Part2.countValid(parsePassports(input)) == 160)
  }
}
