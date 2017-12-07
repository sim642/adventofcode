package eu.sim642.adventofcode2017

import Day7._
import org.scalatest.FunSuite

class Day7Test extends FunSuite {

  test("parseProgram") {
    assert(parseProgram("fwft (72)") == Program("fwft", 72, Seq()))
    assert(parseProgram("fwft (72) -> ktlj") == Program("fwft", 72, Seq("ktlj")))
    assert(parseProgram("fwft (72) -> ktlj, cntj") == Program("fwft", 72, Seq("ktlj", "cntj")))
    assert(parseProgram("fwft (72) -> ktlj, cntj, xhth") == Program("fwft", 72, Seq("ktlj", "cntj", "xhth")))
  }

  test("Part 1 example") {
    val input =
      """pbga (66)
        |xhth (57)
        |ebii (61)
        |havc (66)
        |ktlj (57)
        |fwft (72) -> ktlj, cntj, xhth
        |qoyq (66)
        |padx (45) -> pbga, havc, qoyq
        |tknk (41) -> ugml, padx, fwft
        |jptl (61)
        |ugml (68) -> gyxo, ebii, jptl
        |gyxo (61)
        |cntj (57)""".stripMargin

    assert(bottomProgram(input) == "tknk")
  }

  test("Part 1 input answer") {
    assert(bottomProgram(input) == "gynfwly")
  }

  test("sequence") {
    assert(sequence(Seq()) == Right(Seq()))
    assert(sequence(Seq(Right(1))) == Right(Seq(1)))
    assert(sequence(Seq(Right(1), Right(2))) == Right(Seq(1, 2)))
    assert(sequence(Seq(Left("a"))) == Left("a"))
    assert(sequence(Seq(Right(1), Left("a"))) == Left("a"))
    assert(sequence(Seq(Left("a"), Right(1))) == Left("a"))
    assert(sequence(Seq(Left("a"), Left("b"))) == Left("a"))
  }

  test("sequenceValues") {
    assert(sequenceValues(Map()) == Right(Map()))
    assert(sequenceValues(Map('x' -> Right(1))) == Right(Map('x' -> 1)))
    assert(sequenceValues(Map('x' -> Right(1), 'y' -> Right(2))) == Right(Map('x' -> 1, 'y' -> 2)))
    assert(sequenceValues(Map('x' -> Left("a"))) == Left("a"))
    assert(sequenceValues(Map('x' -> Right(1), 'y' -> Left("a"))) == Left("a"))
    assert(sequenceValues(Map('x' -> Left("a"), 'y' -> Right(1))) == Left("a"))
    assert(sequenceValues(Map('x' -> Left("a"), 'y' -> Left("b"))) == Left("a"))
  }

  test("Part 2 example") {
    val input =
      """pbga (66)
        |xhth (57)
        |ebii (61)
        |havc (66)
        |ktlj (57)
        |fwft (72) -> ktlj, cntj, xhth
        |qoyq (66)
        |padx (45) -> pbga, havc, qoyq
        |tknk (41) -> ugml, padx, fwft
        |jptl (61)
        |ugml (68) -> gyxo, ebii, jptl
        |gyxo (61)
        |cntj (57)""".stripMargin

    assert(correctBalanceWeight(input) == 60)
  }

  test("Part 2 input answer") {
    assert(correctBalanceWeight(input) == 1526)
  }

}
