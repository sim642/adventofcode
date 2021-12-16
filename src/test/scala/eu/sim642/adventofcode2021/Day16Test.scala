package eu.sim642.adventofcode2021

import Day16._
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(parseHexBits("D2FE28") == parseBinBits("110100101111111000101000"))
    assert(parsePacket2(parseBinBits("110100101111111000101000")) == Literal(6, 2021))

    assert(parseHexBits("38006F45291200") == parseBinBits("00111000000000000110111101000101001010010001001000000000"))
    assert(parsePacket2(parseBinBits("00111000000000000110111101000101001010010001001000000000")) == Operator(1, 6, List(Literal(6, 10), Literal(2, 20))))

    assert(parseHexBits("EE00D40C823060") == parseBinBits("11101110000000001101010000001100100000100011000001100000"))
    assert(parsePacket2(parseBinBits("11101110000000001101010000001100100000100011000001100000")) == Operator(7, 3, List(Literal(2, 1), Literal(4, 2), Literal(1, 3))))

    // TODO: version examples structure tests
    assert(sumVersions(parseHexPacket("8A004A801A8002F478")) == 16)
    assert(sumVersions(parseHexPacket("620080001611562C8802118E34")) == 12)
    assert(sumVersions(parseHexPacket("C0015000016115A2E0802F182340")) == 23)
    assert(sumVersions(parseHexPacket("A0016C880162017C3686B18A3D4780")) == 31)
  }

  test("Part 1 input answer") {
    assert(sumVersions(parseHexPacket(input)) == 991)
  }

  test("Part 2 examples") {
    assert(eval(parseHexPacket("C200B40A82")) == 3)
    assert(eval(parseHexPacket("04005AC33890")) == 54)
    assert(eval(parseHexPacket("880086C3E88112")) == 7)
    assert(eval(parseHexPacket("CE00C43D881120")) == 9)
    assert(eval(parseHexPacket("D8005AC2A8F0")) == 1)
    assert(eval(parseHexPacket("F600BC2D8F")) == 0)
    assert(eval(parseHexPacket("9C005AC2F8F0")) == 0)
    assert(eval(parseHexPacket("9C0141080250320F1802104A08")) == 1)
  }

  test("Part 2 input answer") {
    assert(eval(parseHexPacket(input)) == 1264485568252L)
  }
}
