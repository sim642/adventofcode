package eu.sim642.adventofcode2017

import Day9._
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day9Test extends FunSuite with ScalaCheckPropertyChecks {

  test("skipGarbage") {
    val strs = Table(
      "str",
      "<>",
      "<random characters>",
      "<<<<>",
      "<{!>}>",
      "<!!>",
      "<!!!>>",
      "<{o\"i!a,<{i<a>"
    )

    forAll (strs) { str =>
      assert(skipGarbage(str.toList).isEmpty)
    }
  }

  test("stripGarbage") {
    val strStripped = Table(
      ("str", "stripped"),
      ("{}", "{}"),
      ("{{{}}}", "{{{}}}"),
      ("{{},{}}", "{{},{}}"),
      ("{{{},{},{{}}}}", "{{{},{},{{}}}}"),
      ("{<{},{},{{}}>}", "{}"),
      ("{<a>,<a>,<a>,<a>}", "{,,,}"),
      ("{{<a>},{<a>},{<a>},{<a>}}", "{{},{},{},{}}"),
      ("{{<!>},{<!>},{<!>},{<a>}}", "{{}}")
    )

    forAll (strStripped) { (str, stripped) =>
      assert(stripGarbage(str.toList) == stripped.toList)
    }
  }

  test("countGroups") {
    val strGroups = Table(
      ("str", "groups"),
      ("{}", 1),
      ("{{{}}}", 3),
      ("{{},{}}", 3),
      ("{{{},{},{{}}}}", 6),
      ("{<{},{},{{}}>}", 1),
      ("{<a>,<a>,<a>,<a>}", 1),
      ("{{<a>},{<a>},{<a>},{<a>}}", 5),
      ("{{<!>},{<!>},{<!>},{<a>}}", 2)
    )

    forAll (strGroups) { (str, groups) =>
      assert(countGroups(str) == groups)
    }
  }

  test("countScore") {
    val strScore = Table(
      ("str", "score"),
      ("{}", 1),
      ("{{{}}}", 6),
      ("{{},{}}", 5),
      ("{{{},{},{{}}}}", 16),
      ("{<a>,<a>,<a>,<a>}", 1),
      ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9),
      ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9),
      ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
    )

    forAll (strScore) { (str, score) =>
      assert(countScore(str) == score)
    }
  }

  test("Part 1 input answer") {
    assert(countScore(input) == 10050)
  }

  test("stripGarbageCount") {
    val strCounts = Table(
      ("str", "count"),
      ("<>", 0),
      ("<random characters>", 17),
      ("<<<<>", 3),
      ("<{!>}>", 2),
      ("<!!>", 0),
      ("<!!!>>", 0),
      ("<{o\"i!a,<{i<a>", 10)
    )

    forAll (strCounts) { (str, count) =>
      assert(stripGarbageCount(str) == count)
    }
  }

  test("Part 2 input answer") {
    assert(stripGarbageCount(input) == 4482)
  }
}
