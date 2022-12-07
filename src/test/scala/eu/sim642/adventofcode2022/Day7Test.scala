package eu.sim642.adventofcode2022

import Day7._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  val exampleInput =
    """$ cd /
      |$ ls
      |dir a
      |14848514 b.txt
      |8504156 c.dat
      |dir d
      |$ cd a
      |$ ls
      |dir e
      |29116 f
      |2557 g
      |62596 h.lst
      |$ cd e
      |$ ls
      |584 i
      |$ cd ..
      |$ cd ..
      |$ cd d
      |$ ls
      |4060174 j
      |8033020 d.log
      |5626152 d.ext
      |7214296 k""".stripMargin

  test("Part 1 examples") {
    assert(totalSmallDirSizes(parseCommands(exampleInput)) == 95437)
  }

  test("Part 1 input answer") {
    assert(totalSmallDirSizes(parseCommands(input)) == 1077191)
  }

  test("Part 2 examples") {
    assert(findDeleteDirSize(parseCommands(exampleInput)) == 24933642)
  }

  test("Part 2 input answer") {
    assert(findDeleteDirSize(parseCommands(input)) == 5649896)
  }
}
