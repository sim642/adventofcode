package eu.sim642.adventofcode2017

import Day9._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day9Test extends FunSuite with PropertyChecks {

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
}
