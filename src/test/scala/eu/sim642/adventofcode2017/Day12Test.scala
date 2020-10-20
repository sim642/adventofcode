package eu.sim642.adventofcode2017

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  val exampleInput = """0 <-> 2
                       |1 <-> 1
                       |2 <-> 0, 3, 4
                       |3 <-> 2, 4
                       |4 <-> 2, 3, 6
                       |5 <-> 6
                       |6 <-> 4, 5""".stripMargin

  test("parseNode") {
    assert(parseNode("0 <-> 2") == 0 -> Seq(2))
    assert(parseNode("1 <-> 1") == 1 -> Seq(1))
    assert(parseNode("2 <-> 0, 3, 4") == 2 -> Seq(0, 3, 4))
  }

  test("Part 1 example") {
    assert(groupSize(exampleInput) == 6)
  }

  test("Part 1 input answer") {
    assert(groupSize(input) == 130)
  }

  test("Part 2 example") {
    assert(bfsGroups(parseNodes(exampleInput)) == Set(Set(0, 2, 3, 4, 5, 6), Set(1)))

    assert(groupCount(exampleInput) == 2)
  }

  test("Part 2 input answer") {
    assert(groupCount(input) == 189)
  }
}
