package eu.sim642.adventofcode2021

import Day16._
import Day16Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day16Test extends Suites(
  new BaseTest,
  new RecursiveDescentSolutionTest,
  new ParserCombinatorSolutionTest,
)

object Day16Test {

  class BaseTest extends AnyFunSuite with ScalaCheckPropertyChecks {
    test("parseBits") {
      val hexExpectedBin = Table(
        ("hex", "expectedBin"),
        ("D2FE28", "110100101111111000101000"),
        ("38006F45291200", "00111000000000000110111101000101001010010001001000000000"),
        ("EE00D40C823060", "11101110000000001101010000001100100000100011000001100000"),
      )

      forAll(hexExpectedBin) { (hex, expectedBin) =>
        assert(parseHexBits(hex) == parseBinBits(expectedBin))
      }
    }
  }

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite with ScalaCheckPropertyChecks {
    import solution._

    test("Part 1 examples") {
      val hexExpectedPacket = Table(
        ("hex", "expectedPacket"),
        ("D2FE28", Literal(6, 2021)),
        ("38006F45291200", Operator(1, 6, List(Literal(6, 10), Literal(2, 20)))),
        ("EE00D40C823060", Operator(7, 3, List(Literal(2, 1), Literal(4, 2), Literal(1, 3)))),
        ("8A004A801A8002F478", Operator(4, 2, List(Operator(1, 2, List(Operator(5, 2, List(Literal(6, 15)))))))),
        ("620080001611562C8802118E34", Operator(3, 0, List(Operator(0, 0, List(Literal(0, 10), Literal(5, 11))), Operator(1, 0, List(Literal(0, 12), Literal(3, 13)))))),
        ("C0015000016115A2E0802F182340", Operator(6, 0, List(Operator(0, 0, List(Literal(0, 10), Literal(6, 11))), Operator(4, 0, List(Literal(7, 12), Literal(0, 13)))))),
        ("A0016C880162017C3686B18A3D4780", Operator(5, 0, List(Operator(1, 0, List(Operator(3, 0,  List(Literal(7, 6), Literal(6, 6), Literal(5, 12), Literal(2, 15), Literal(2, 15)))))))),
      )

      forAll(hexExpectedPacket) { (hex, expectedPacket) =>
        assert(parseHex(hex) == expectedPacket)
      }

      val hexExpectedSumVersions = Table(
        ("hex", "expectedSumVersions"),
        ("8A004A801A8002F478", 16),
        ("620080001611562C8802118E34", 12),
        ("C0015000016115A2E0802F182340", 23),
        ("A0016C880162017C3686B18A3D4780", 31),
      )

      forAll(hexExpectedSumVersions) { (hex, expectedSumVersion) =>
        assert(sumVersions(parseHex(hex)) == expectedSumVersion)
      }
    }

    test("Part 1 input answer") {
      assert(sumVersions(parseHex(input)) == 991)
    }

    test("Part 2 examples") {
      val hexExpectedEvals = Table(
        ("hex", "expectedEval"),
        ("C200B40A82", 3),
        ("04005AC33890", 54),
        ("880086C3E88112", 7),
        ("CE00C43D881120", 9),
        ("D8005AC2A8F0", 1),
        ("F600BC2D8F", 0),
        ("9C005AC2F8F0", 0),
        ("9C0141080250320F1802104A08", 1),
      )

      forAll(hexExpectedEvals) { (hex, expectedEval) =>
        assert(eval(parseHex(hex)) == expectedEval)
      }
    }

    test("Part 2 input answer") {
      assert(eval(parseHex(input)) == 1264485568252L)
    }
  }

  class RecursiveDescentSolutionTest extends SolutionTest(RecursiveDescentSolution)

  class ParserCombinatorSolutionTest extends SolutionTest(ParserCombinatorSolution)
}
