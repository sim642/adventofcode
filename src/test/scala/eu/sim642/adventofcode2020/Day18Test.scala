package eu.sim642.adventofcode2020

import Day18._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day18Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Part 1 parseExpr") {
    val inputExpectedExpr = Table(
      ("input", "expectedExpr"),
      ("1 + 2 * 3 + 4 * 5 + 6", Add(Mul(Add(Mul(Add(Num(1), Num(2)), Num(3)), Num(4)), Num(5)), Num(6))),
      ("1 + (2 * 3) + (4 * (5 + 6))", Add(Add(Num(1), Mul(Num(2), Num(3))), Mul(Num(4), Add(Num(5), Num(6))))),
    )

    forAll(inputExpectedExpr) { (input, expectedExpr) =>
      assert(Part1.parseExpr(input) == expectedExpr)
    }
  }

  test("Part 1 examples") {
    val inputExpectedValue = Table(
      ("input", "expectedValue"),
      ("1 + 2 * 3 + 4 * 5 + 6", 71),
      ("1 + (2 * 3) + (4 * (5 + 6))", 51),
      ("2 * 3 + (4 * 5)", 26),
      ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437),
      ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240),
      ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632)
    )

    forAll(inputExpectedValue) { (input, expectedValue) =>
      assert(eval(Part1.parseExpr(input)) == expectedValue)
    }
  }

  test("Part 1 input answer") {
    assert(sumEvals(Part1.parseExprs(input)) == 1451467526514L)
  }

  test("Part 2 parseExpr") {
    val inputExpectedExpr = Table(
      ("input", "expectedExpr"),
      ("1 + 2 * 3 + 4 * 5 + 6", Mul(Mul(Add(Num(1), Num(2)), Add(Num(3), Num(4))), Add(Num(5), Num(6)))),
      ("1 + (2 * 3) + (4 * (5 + 6))", Add(Add(Num(1), Mul(Num(2), Num(3))), Mul(Num(4), Add(Num(5), Num(6))))),
    )

    forAll(inputExpectedExpr) { (input, expectedExpr) =>
      assert(Part2.parseExpr(input) == expectedExpr)
    }
  }

  test("Part 2 examples") {
    val inputExpectedValue = Table(
      ("input", "expectedValue"),
      ("1 + 2 * 3 + 4 * 5 + 6", 231),
      ("1 + (2 * 3) + (4 * (5 + 6))", 51),
      ("2 * 3 + (4 * 5)", 46),
      ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445),
      ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060),
      ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340)
    )

    forAll(inputExpectedValue) { (input, expectedValue) =>
      assert(eval(Part2.parseExpr(input)) == expectedValue)
    }
  }

  test("Part 2 input answer") {
    assert(sumEvals(Part2.parseExprs(input)) == 224973686321527L)
  }
}
