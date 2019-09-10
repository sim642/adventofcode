package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day22._

class Day22Test extends FunSuite {

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
