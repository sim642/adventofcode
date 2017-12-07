package eu.sim642.adventofcode2017

import Day7._
import org.scalatest.FunSuite

class Day7Test extends FunSuite {

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

    assert(wrongHeight(input) == 60)
  }

  test("Part 2 input answer") {
    assert(wrongHeight(input) == 1526)
  }

}
