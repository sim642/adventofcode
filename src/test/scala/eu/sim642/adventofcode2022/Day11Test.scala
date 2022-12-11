package eu.sim642.adventofcode2022

import Day11._
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {

  val exampleInput =
    """Monkey 0:
      |  Starting items: 79, 98
      |  Operation: new = old * 19
      |  Test: divisible by 23
      |    If true: throw to monkey 2
      |    If false: throw to monkey 3
      |
      |Monkey 1:
      |  Starting items: 54, 65, 75, 74
      |  Operation: new = old + 6
      |  Test: divisible by 19
      |    If true: throw to monkey 2
      |    If false: throw to monkey 0
      |
      |Monkey 2:
      |  Starting items: 79, 60, 97
      |  Operation: new = old * old
      |  Test: divisible by 13
      |    If true: throw to monkey 1
      |    If false: throw to monkey 3
      |
      |Monkey 3:
      |  Starting items: 74
      |  Operation: new = old + 3
      |  Test: divisible by 17
      |    If true: throw to monkey 0
      |    If false: throw to monkey 1""".stripMargin

  test("Part 1 examples") {
    assert(Part1.monkeyBusiness(parseMonkeys(exampleInput)) == 10605)
  }

  test("Part 1 input answer") {
    assert(Part1.monkeyBusiness(parseMonkeys(input)) == 69918)
  }

  test("Part 2 examples") {
    assert(Part2.monkeyBusiness(parseMonkeys(exampleInput)) == 2713310158L)
  }

  test("Part 2 input answer") {
    assert(Part2.monkeyBusiness(parseMonkeys(input)) == 19573408701L)
  }

  test("Part 3 examples") {
    assert(Part3.monkeyBusiness(parseMonkeys(exampleInput)) == 900260526315763L)
  }

  test("Part 3 input answer") {
    assert(Part3.monkeyBusiness(parseMonkeys(input)) == 2419199999999810L)
  }
}
