package eu.sim642.adventofcode2018

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  val exampleInput =
    """initial state: #..#.#..##......###...###
      |
      |...## => #
      |..#.. => #
      |.#... => #
      |.#.#. => #
      |.#.## => #
      |.##.. => #
      |.#### => #
      |#.#.# => #
      |#.### => #
      |##.#. => #
      |##.## => #
      |###.. => #
      |###.# => #
      |####. => #""".stripMargin

  test("Part 1 examples") {
    assert(sumPlantsSimulate(exampleInput) == 325)
  }

  test("Part 1 input answer") {
    assert(sumPlantsSimulate(input) == 3472)
  }

  test("Part 2 input answer") {
    assert(sumPlantsCycle(input) == 2600000000919L)
  }
}
