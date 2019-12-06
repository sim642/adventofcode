package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day6._

class Day6Test extends FunSuite {

  val exampleInput =
    """COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L""".stripMargin

  val exampleInput2 =
    """COM)B
      |B)C
      |C)D
      |D)E
      |E)F
      |B)G
      |G)H
      |D)I
      |E)J
      |J)K
      |K)L
      |K)YOU
      |I)SAN""".stripMargin

  test("Part 1 examples") {
    assert(countOrbits(parseParentMap(exampleInput)) == 42)
  }

  test("Part 1 input answer") {
    assert(countOrbits(parseParentMap(input)) == 308790)
  }

  test("Part 2 examples") {
    assert(countOrbitalTransfers(parseParentMap(exampleInput2)) == 4)
  }

  test("Part 2 input answer") {
    assert(countOrbitalTransfers(parseParentMap(input)) == 472)
  }
}
