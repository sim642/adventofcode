package eu.sim642.adventofcode2015

import Day22._
import org.scalatest.funsuite.AnyFunSuite

class Day22Test extends AnyFunSuite {

  test("Part 1 examples") {
    val initialState = State(myHitpoints = 10, myMana = 250,
                             enemyHitpoints = 13, enemyDamage = 8)
    assert(leastWinManaUsed(initialState) == 173 + 53)

    val initialState2 = initialState.copy(enemyHitpoints = 14)
    assert(leastWinManaUsed(initialState2) == 229 + 113 + 73 + 173 + 53)
  }

  test("Part 1 input answer") {
    assert(leastWinManaUsed(input) == 1269)
  }

  test("Part 2 input answer") {
    assert(leastWinManaUsedHard(input) == 1309)
  }
}
