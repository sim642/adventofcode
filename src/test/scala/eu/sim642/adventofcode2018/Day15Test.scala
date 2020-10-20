package eu.sim642.adventofcode2018

import Day15._
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite {

  test("Move example 1") {
    val moveInput =
      """#######
        |#E..G.#
        |#...#.#
        |#.G.#G#
        |#######""".stripMargin

    implicit val (grid, units) = parseInput(moveInput)

    val unit = CombatUnit(Elf, Pos(1, 1))
    val targets = getTargets(unit)
    assert(targets == Set(
      CombatUnit(Goblin, Pos(4, 1)),
      CombatUnit(Goblin, Pos(2, 3)),
      CombatUnit(Goblin, Pos(5, 3)),
    ))

    val inRange = getInRange(targets)
    assert(inRange == Set(
      Pos(3, 1), Pos(5, 1),
      Pos(1, 3), Pos(2, 2), Pos(3, 3),
      Pos(5, 2)
    ))

    val reachable = getReachable(unit, inRange)
    assert(reachable == Map(
      Pos(3, 1) -> 2,
      Pos(2, 2) -> 2,
      Pos(1, 3) -> 2,
      //Pos(3, 3) -> 4 // BFS ends early now
    ))

    val nearest = getNearest(reachable)
    assert(nearest == Set(
      Pos(3, 1),
      Pos(2, 2),
      Pos(1, 3),
    ))

    val chosen = getChosen(nearest)
    assert(chosen == Pos(3, 1))
  }

  test("Move example 2") {
    val moveInput =
      """#######
        |#.E...#
        |#.....#
        |#...G.#
        |#######""".stripMargin

    implicit val (grid, units) = parseInput(moveInput)

    val unit = CombatUnit(Elf, Pos(2, 1))
    val targets = getTargets(unit)
    assert(targets == Set(
      CombatUnit(Goblin, Pos(4, 3)),
    ))

    val inRange = getInRange(targets)
    assert(inRange == Set(
      Pos(4, 2), Pos(3, 3), Pos(5, 3)
    ))

    val reachable = getReachable(unit, inRange)

    val nearest = getNearest(reachable)
    assert(nearest == Set(
      Pos(4, 2),
      Pos(3, 3),
    ))

    val chosen = getChosen(nearest)
    assert(chosen == Pos(4, 2))

    val step = getStep(chosen, unit)
    assert(step == Pos(3, 1))
  }

  val exampleInput1 =
    """#######
      |#.G...#
      |#...EG#
      |#.#.#G#
      |#..G#E#
      |#.....#
      |#######""".stripMargin

  val exampleInput2 =
    """#######
      |#G..#E#
      |#E#E.E#
      |#G.##.#
      |#...#E#
      |#...E.#
      |#######""".stripMargin

  val exampleInput3 =
    """#######
      |#E..EG#
      |#.#G.E#
      |#E.##E#
      |#G..#.#
      |#..E#.#
      |#######""".stripMargin

  val exampleInput4 =
    """#######
      |#E.G#.#
      |#.#G..#
      |#G.#.G#
      |#G..#.#
      |#...E.#
      |#######""".stripMargin

  val exampleInput5 =
    """#######
      |#.E...#
      |#.#..G#
      |#.###.#
      |#E#G#G#
      |#...#G#
      |#######""".stripMargin

  val exampleInput6 =
    """#########
      |#G......#
      |#.E.#...#
      |#..##..G#
      |#...##..#
      |#...#...#
      |#.G...G.#
      |#.....G.#
      |#########""".stripMargin

  test("Part 1 examples") {
    assert(combatOutcome(exampleInput1) == 27730)
    assert(combatOutcome(exampleInput2) == 36334)
    assert(combatOutcome(exampleInput3) == 39514)
    assert(combatOutcome(exampleInput4) == 27755)
    assert(combatOutcome(exampleInput5) == 28944)
    assert(combatOutcome(exampleInput6) == 18740)
  }

  test("Part 1 input answer") {
    assert(combatOutcome(input) == 197025)
  }

  test("Part 2 examples") {
    assert(combatOutcomeElfWin(exampleInput1) == 4988)
    // wrong numbering in part 2 description
    assert(combatOutcomeElfWin(exampleInput3) == 31284)
    assert(combatOutcomeElfWin(exampleInput4) == 3478)
    assert(combatOutcomeElfWin(exampleInput5) == 6474)
    assert(combatOutcomeElfWin(exampleInput6) == 1140)
  }

  test("Part 2 input answer") {
    assert(combatOutcomeElfWin(input) == 44423)
  }
}
