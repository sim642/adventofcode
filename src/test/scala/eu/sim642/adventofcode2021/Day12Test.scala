package eu.sim642.adventofcode2021

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  val exampleInput1 =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end""".stripMargin

  val exampleInput2 =
    """dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc""".stripMargin

  val exampleInput3 =
    """fs-end
      |he-DX
      |fs-he
      |start-DX
      |pj-DX
      |end-zg
      |zg-sl
      |zg-pj
      |pj-he
      |RW-he
      |fs-DX
      |pj-RW
      |zg-RW
      |start-pj
      |he-WI
      |zg-he
      |pj-fs
      |start-RW""".stripMargin

  test("Part 1 examples") {
    assert(countPaths(parseCaveMap(exampleInput1)) == 10)
    assert(countPaths(parseCaveMap(exampleInput2)) == 19)
    assert(countPaths(parseCaveMap(exampleInput3)) == 226)
  }

  test("Part 1 input answer") {
    assert(countPaths(parseCaveMap(input)) == 4659)
  }
}
