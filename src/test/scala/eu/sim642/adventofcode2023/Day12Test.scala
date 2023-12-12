package eu.sim642.adventofcode2023

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  private val exampleInput =
    """???.### 1,1,3
      |.??..??...?##. 1,1,3
      |?#?#?#?#?#?#?#? 1,3,1,6
      |????.#...#... 4,1,1
      |????.######..#####. 1,6,5
      |?###???????? 3,2,1""".stripMargin

  test("Part 1 examples") {
    assert(sumPossibleArrangements(parseRecords(exampleInput)) == 21)
  }

  test("Part 1 input answer") {
    assert(sumPossibleArrangements(parseRecords(input)) == 8270)
  }
}
