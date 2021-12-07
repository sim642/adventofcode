package eu.sim642.adventofcode2021

import Day7._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  val exampleInput = "16,1,2,0,4,2,7,1,2,14"

  test("Part 1 examples") {
    val exampleCrabs = parseCrabs(exampleInput)
    assert(Part1.alignPosFuel(exampleCrabs, 2) == 37)
    assert(Part1.alignPosFuel(exampleCrabs, 1) == 41)
    assert(Part1.alignPosFuel(exampleCrabs, 3) == 39)
    assert(Part1.alignPosFuel(exampleCrabs, 10) == 71)
    assert(Part1.minAlignPosFuel(exampleCrabs) == 37)
  }

  test("Part 1 input answer") {
    assert(Part1.minAlignPosFuel(parseCrabs(input)) == 336721)
  }

  test("Part 2 examples") {
    val exampleCrabs = parseCrabs(exampleInput)
    assert(Part2.alignPosFuel(exampleCrabs, 5) == 168)
    assert(Part2.alignPosFuel(exampleCrabs, 2) == 206)
    assert(Part2.minAlignPosFuel(exampleCrabs) == 168)
  }

  test("Part 2 input answer") {
    assert(Part2.minAlignPosFuel(parseCrabs(input)) == 91638945)
  }
}
