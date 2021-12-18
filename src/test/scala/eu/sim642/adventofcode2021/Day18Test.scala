package eu.sim642.adventofcode2021

import Day18._
import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite {

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

  test("Part 1 examples") {
    assert(parseNumber("[1,2]") + parseNumber("[[3,4],5]") == parseNumber("[[1,2],[[3,4],5]]"))
    assert(explode(parseNumber("[[[[[9,8],1],2],3],4]")) == Some(parseNumber("[[[[0,9],2],3],4]")))
    assert(explode(parseNumber("[7,[6,[5,[4,[3,2]]]]]")) == Some(parseNumber("[7,[6,[5,[7,0]]]]")))
    assert(explode(parseNumber("[[6,[5,[4,[3,2]]]],1]")) == Some(parseNumber("[[6,[5,[7,0]]],3]")))
    assert(explode(parseNumber("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")) == Some(parseNumber("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")))
    assert(explode(parseNumber("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")) == Some(parseNumber("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")))
    assert(split(parseNumber("10")) == Some(parseNumber("[5,5]")))
    assert(split(parseNumber("11")) == Some(parseNumber("[5,6]")))
    assert(split(parseNumber("12")) == Some(parseNumber("[6,6]")))

    assert(parseNumber("[[[[4,3],4],4],[7,[[8,4],9]]]") + parseNumber("[1,1]") == parseNumber("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"))

    assert(addNumbers(parseNumbers(exampleInput2)) == parseNumber("[[[[1,1],[2,2]],[3,3]],[4,4]]"))
    assert(addNumbers(parseNumbers(exampleInput3)) == parseNumber("[[[[3,0],[5,3]],[4,4]],[5,5]]"))
    assert(addNumbers(parseNumbers(exampleInput4)) == parseNumber("[[[[5,0],[7,4]],[5,5]],[6,6]]"))


    assert(parseNumber("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]") + parseNumber("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]") == parseNumber("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"))
    assert(parseNumber("[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]") + parseNumber("[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]") == parseNumber("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"))
    assert(parseNumber("[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]") + parseNumber("[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]") == parseNumber("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]"))
    assert(parseNumber("[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]") + parseNumber("[7,[5,[[3,8],[1,4]]]]") == parseNumber("[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]"))
    assert(parseNumber("[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]") + parseNumber("[[2,[2,2]],[8,[8,1]]]") == parseNumber("[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]"))
    assert(parseNumber("[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]") + parseNumber("[2,9]") == parseNumber("[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]"))
    assert(parseNumber("[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]") + parseNumber("[1,[[[9,3],9],[[9,0],[0,7]]]]") == parseNumber("[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]"))
    assert(parseNumber("[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]") + parseNumber("[[[5,[7,4]],7],1]") == parseNumber("[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]"))
    assert(parseNumber("[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]") + parseNumber("[[[[4,2],2],6],[8,7]]") == parseNumber("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))

    assert(addNumbers(parseNumbers(exampleInput5)) == parseNumber("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"))

    assert(parseNumber("[9,1]").magnitude == 29)
    assert(parseNumber("[1,9]").magnitude == 21)
    assert(parseNumber("[[9,1],[1,9]]").magnitude == 129)
    assert(parseNumber("[[1,2],[[3,4],5]]").magnitude == 143)
    assert(parseNumber("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").magnitude == 1384)
    assert(parseNumber("[[[[1,1],[2,2]],[3,3]],[4,4]]").magnitude == 445)
    assert(parseNumber("[[[[3,0],[5,3]],[4,4]],[5,5]]").magnitude == 791)
    assert(parseNumber("[[[[5,0],[7,4]],[5,5]],[6,6]]").magnitude == 1137)
    assert(parseNumber("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude == 3488)

    assert(addNumbers(parseNumbers(exampleInput6)) == parseNumber("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"))
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
