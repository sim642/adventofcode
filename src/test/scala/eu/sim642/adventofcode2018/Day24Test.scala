package eu.sim642.adventofcode2018

import Day24._
import org.scalatest.funsuite.AnyFunSuite

class Day24Test extends AnyFunSuite {

  val exampleInput =
    """Immune System:
      |17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
      |989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3
      |
      |Infection:
      |801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
      |4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4""".stripMargin

  test("parseGroup") {
    val s = "18 units each with 729 hit points (weak to fire; immune to cold, slashing) with an attack that does 8 radiation damage at initiative 10"
    assert(parseGroup(s, 1, ImmuneSystem) == Group(1, ImmuneSystem, 18, 729, 8, "radiation", 10, Set("fire"), Set("cold", "slashing")))
  }

  test("parseInput") {
    parseInput(exampleInput)
    parseInput(input)
  }

  test("Part 1 examples") {
    assert(combatToWin(parseInput(exampleInput)) == 5216)
  }

  test("Part 1 input answer") {
    assert(combatToWin(parseInput(input)) == 19295)
  }

  test("Part 2 examples") {
    assert(combat(boostGroups(parseInput(exampleInput), 1570)) == Some(ImmuneSystem, 51))
    assert(smallestBoostedCombat(parseInput(exampleInput)) == 51)
  }

  test("Part 2 input answer") {
    assert(smallestBoostedCombat(parseInput(input)) == 12084)
  }
}
