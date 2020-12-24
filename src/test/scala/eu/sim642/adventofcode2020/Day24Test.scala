package eu.sim642.adventofcode2020

import Day24._
import org.scalatest.funsuite.AnyFunSuite

class Day24Test extends AnyFunSuite {

  val exampleInput =
    """sesenwnenenewseeswwswswwnenewsewsw
      |neeenesenwnwwswnenewnwwsewnenwseswesw
      |seswneswswsenwwnwse
      |nwnwneseeswswnenewneswwnewseswneseene
      |swweswneswnenwsewnwneneseenw
      |eesenwseswswnenwswnwnwsewwnwsene
      |sewnenenenesenwsewnenwwwse
      |wenwwweseeeweswwwnwwe
      |wsweesenenewnwwnwsenewsenwwsesesenwne
      |neeswseenwwswnwswswnw
      |nenwswwsewswnenenewsenwsenwnesesenew
      |enewnwewneswsewnwswenweswnenwsenwsw
      |sweneswneswneneenwnewenewwneswswnese
      |swwesenesewenwneswnwwneseswwne
      |enesenwswwswneneswsenwnewswseenwsese
      |wnwnesenesenenwwnenwsewesewsesesew
      |nenewswnwewswnenesenwnesewesw
      |eneswnwswnwsenenwnwnwwseeswneewsenese
      |neswnwewnwnwseenwseesewsenwsweewe
      |wseweeenwnesenwwwswnew""".stripMargin

  test("Part 1 examples") {
    assert(countBlackTiles(parseDirections(exampleInput)) == 10)
  }

  test("Part 1 input answer") {
    assert(countBlackTiles(parseDirections(input)) == 277)
  }

  test("Part 2 examples") {
    assert(countBlackTilesAfter(parseDirections(exampleInput)) == 2208)
  }

  test("Part 2 input answer") {
    assert(countBlackTilesAfter(parseDirections(input)) == 3531)
  }
}
