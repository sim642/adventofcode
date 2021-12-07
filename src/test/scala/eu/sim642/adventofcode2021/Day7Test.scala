package eu.sim642.adventofcode2021

import Day7._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  val exampleInput = "16,1,2,0,4,2,7,1,2,14"

  test("Part 1 examples") {
    val exampleCrabs = parseCrabs(exampleInput)
    assert(alignPosFuel(exampleCrabs, 2) == 37)
    assert(alignPosFuel(exampleCrabs, 1) == 41)
    assert(alignPosFuel(exampleCrabs, 3) == 39)
    assert(alignPosFuel(exampleCrabs, 10) == 71)
    assert(minAlignPosFuel(exampleCrabs) == 37)
  }

  test("Part 1 input answer") {
    assert(minAlignPosFuel(parseCrabs(input)) == 336721)
  }
}
