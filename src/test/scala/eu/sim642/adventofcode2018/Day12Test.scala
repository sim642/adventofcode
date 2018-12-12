package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day12._

class Day12Test extends FunSuite {

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
    assert(sumPlants(exampleInput) == 325)
  }

  test("Part 1 input answer") {
    assert(sumPlants(input) == 3472)
  }

  test("Part 2 input answer") {
    assert(sumPlants2(input) == 2600000000919L)
  }
}
