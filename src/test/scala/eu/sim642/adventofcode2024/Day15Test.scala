package eu.sim642.adventofcode2024

import Day15._
import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite {

  val exampleInput =
    """##########
      |#..O..O.O#
      |#......O.#
      |#.OO..O.O#
      |#..O@..O.#
      |#O#..O...#
      |#O..O..O.#
      |#.OO.O.OO#
      |#....O...#
      |##########
      |
      |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
      |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
      |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
      |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
      |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
      |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
      |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
      |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
      |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
      |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin

  val exampleInput2 =
    """########
      |#..O.O.#
      |##@.O..#
      |#...O..#
      |#.#.O..#
      |#...O..#
      |#......#
      |########
      |
      |<^^>>>vv<v>>v<<""".stripMargin

  val exampleInput3 =
    """#######
      |#...#.#
      |#.....#
      |#..OO@#
      |#..O..#
      |#.....#
      |#######
      |
      |<vv<<^^<<^^""".stripMargin

  test("Part 1 examples") {
    assert(Part1.sumMovesBoxGps(parseInput(exampleInput2)) == 2028)
    assert(Part1.sumMovesBoxGps(parseInput(exampleInput)) == 10092)
    assert(Part1.sumMovesBoxGps(parseInput(exampleInput3)) == 908) // from glguy
  }

  test("Part 1 input answer") {
    assert(Part1.sumMovesBoxGps(parseInput(input)) == 1476771)
  }

  test("Part 2 examples") {
    assert(Part2.sumMovesBoxGps(parseInput(exampleInput3)) == 618) // from glguy
    assert(Part2.sumMovesBoxGps(parseInput(exampleInput2)) == 1751) // from glguy
    assert(Part2.sumMovesBoxGps(parseInput(exampleInput)) == 9021)
  }

  test("Part 2 input answer") {
    assert(Part2.sumMovesBoxGps(parseInput(input)) == 1468005)
  }
}
