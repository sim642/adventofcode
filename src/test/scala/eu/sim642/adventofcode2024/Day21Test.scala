package eu.sim642.adventofcode2024

import Day21._
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {

  val exampleInput =
    """029A
      |980A
      |179A
      |456A
      |379A""".stripMargin

  test("Part 1 examples") {
    assert(shortestSequenceLength2("029A", 0) == "<A^A>^^AvvvA".length)
    assert(shortestSequenceLength2("029A", 1) == "v<<A>>^A<A>AvA<^AA>A<vAAA>^A".length)
    assert(shortestSequenceLength2("029A", 2) == "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A".length)

    assert(sumCodeComplexity(parseCodes(exampleInput), 2) == 126384)
  }

  test("Part 1 input answer") {
    assert(sumCodeComplexity(parseCodes(input), 2) == 157892)
  }

  test("Part 2 input answer") {
    assert(sumCodeComplexity(parseCodes(input), 25) == 197015606336332L)
  }
}
