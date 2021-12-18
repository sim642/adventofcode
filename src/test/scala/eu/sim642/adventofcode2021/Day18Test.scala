package eu.sim642.adventofcode2021

import Day18._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day18Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleInput1 =
    """[1,2]
      |[[1,2],3]
      |[9,[8,7]]
      |[[1,9],[8,5]]
      |[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
      |[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
      |[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]""".stripMargin

  val exampleInput2 =
    """[1,1]
      |[2,2]
      |[3,3]
      |[4,4]""".stripMargin

  val exampleInput3 =
    """[1,1]
      |[2,2]
      |[3,3]
      |[4,4]
      |[5,5]""".stripMargin

  val exampleInput4 =
    """[1,1]
      |[2,2]
      |[3,3]
      |[4,4]
      |[5,5]
      |[6,6]""".stripMargin

  val exampleInput5 =
    """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
      |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
      |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
      |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
      |[7,[5,[[3,8],[1,4]]]]
      |[[2,[2,2]],[8,[8,1]]]
      |[2,9]
      |[1,[[[9,3],9],[[9,0],[0,7]]]]
      |[[[5,[7,4]],7],1]
      |[[[[4,2],2],6],[8,7]]""".stripMargin

  val exampleInput6 =
    """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
      |[[[5,[2,8]],4],[5,[[9,9],0]]]
      |[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
      |[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
      |[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
      |[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
      |[[[[5,4],[7,7]],8],[[8,3],8]]
      |[[9,3],[[9,9],[6,[4,9]]]]
      |[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
      |[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]""".stripMargin

  test("explode") {
    val numberExpectedExplode = Table(
      ("number", "expectedExplode"),
      ("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]"),
      ("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]"),
      ("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]"),
      ("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"),
      ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"),
    )

    forAll(numberExpectedExplode) { (number, expectedExplode) =>
      assert(explode(parseNumber(number)).contains(parseNumber(expectedExplode)))
    }
  }

  test("split") {
    val numberExpectedSplit = Table(
      ("number", "expectedSplit"),
      ("10", "[5,5]"),
      ("11", "[5,6]"),
      ("12", "[6,6]"),
    )

    forAll(numberExpectedSplit) { (number, expectedSplit) =>
      assert(split(parseNumber(number)).contains(parseNumber(expectedSplit)))
    }
  }

  test("magnitude") {
    val numberExpectedMagnitude = Table(
      ("number", "expectedMagnitude"),
      ("[9,1]", 29),
      ("[1,9]", 21),
      ("[[9,1],[1,9]]", 129),
      ("[[1,2],[[3,4],5]]", 143),
      ("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384),
      ("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445),
      ("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791),
      ("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137),
      ("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488),
    )

    forAll(numberExpectedMagnitude) { (number, expectedMagnitude) =>
      assert(parseNumber(number).magnitude == expectedMagnitude)
    }
  }

  test("+") {
    assert(parseNumber("[1,2]") + parseNumber("[[3,4],5]") == parseNumber("[[1,2],[[3,4],5]]"))
    assert(parseNumber("[[[[4,3],4],4],[7,[[8,4],9]]]") + parseNumber("[1,1]") == parseNumber("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

    val addNumberExpectedSum = Table(
      ("addNumber", "expectedSum"),
      ("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]", "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"),
      ("[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]", "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"),
      ("[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]", "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"),
      ("[7,[5,[[3,8],[1,4]]]]", "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"),
      ("[[2,[2,2]],[8,[8,1]]]", "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"),
      ("[2,9]", "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"),
      ("[1,[[[9,3],9],[[9,0],[0,7]]]]", "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"),
      ("[[[5,[7,4]],7],1]", "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"),
      ("[[[[4,2],2],6],[8,7]]", "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"),
    )

    var sum = parseNumber("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]")
    forAll(addNumberExpectedSum) { (addNumber, expectedSum) =>
      sum += parseNumber(addNumber)
      assert(sum == parseNumber(expectedSum))
    }
  }

  test("addNumbers") {
    val inputExpectedSum = Table(
      ("input", "expectedSum"),
      (exampleInput2, "[[[[1,1],[2,2]],[3,3]],[4,4]]"),
      (exampleInput3, "[[[[3,0],[5,3]],[4,4]],[5,5]]"),
      (exampleInput4, "[[[[5,0],[7,4]],[5,5]],[6,6]]"),
      (exampleInput5, "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"),
      (exampleInput6, "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"),
    )

    forAll(inputExpectedSum) { (input, expectedSum) =>
      assert(addNumbers(parseNumbers(input)) == parseNumber(expectedSum))
    }
  }

  test("Part 1 examples") {
    assert(addNumbersMagnitude(parseNumbers(exampleInput6)) == 4140)
  }

  test("Part 1 input answer") {
    assert(addNumbersMagnitude(parseNumbers(input)) == 4184)
  }

  test("Part 2 examples") {
    assert(largestTwoMagnitude(parseNumbers(exampleInput6)) == 3993)
  }

  test("Part 2 input answer") {
    assert(largestTwoMagnitude(parseNumbers(input)) == 4731)
  }
}
