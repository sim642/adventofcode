package eu.sim642.adventofcode2017

import Day8._
import org.scalatest.FunSuite

class Day8Test extends FunSuite {

  test("parseInstruction") {
    assert(parseInstruction("b inc 5 if a > 1") == Instruction("b", Inc(5), Condition("a", Greater, 1)))
    assert(parseInstruction("a inc 1 if b < 5") == Instruction("a", Inc(1), Condition("b", Less, 5)))
    assert(parseInstruction("c dec -10 if a >= 1") == Instruction("c", Dec(-10), Condition("a", GreaterEq, 1)))
    assert(parseInstruction("c inc -20 if c == 10") == Instruction("c", Inc(-20), Condition("c", Eq, 10)))
  }

  test("Part 1 example") {
    val instructionsStr = """b inc 5 if a > 1
                            |a inc 1 if b < 5
                            |c dec -10 if a >= 1
                            |c inc -20 if c == 10""".stripMargin
    assert(largestValueAfter(instructionsStr) == 1)
  }

  test("Part 1 input answer") {
    assert(largestValueAfter(input) == 5752)
  }

  test("Part 2 example") {
    val instructionsStr = """b inc 5 if a > 1
                            |a inc 1 if b < 5
                            |c dec -10 if a >= 1
                            |c inc -20 if c == 10""".stripMargin
    assert(largestValueDuring(instructionsStr) == 10)
  }

  test("Part 2 input answer") {
    assert(largestValueDuring(input) == 6366)
  }

}
