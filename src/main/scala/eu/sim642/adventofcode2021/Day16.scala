package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.parsing.{ExtraParsers, ListParsers}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object Day16 {

  type Bits = List[Boolean]

  def bits2int(bits: Bits): Long = {
    bits.foldLeft(0L)({ case (acc, bit) => (acc << 1) | (if (bit) 1 else 0) })
  }


  sealed trait Packet {
    val version: Int
  }
  case class Literal(version: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Int, subpackets: List[Packet]) extends Packet


  sealed trait Solution {
    def parse(bits: Bits): Packet

    def parseHex(input: String): Packet = parse(parseHexBits(input))
  }

  object RecursiveDescentSolution extends Solution {

    def parseLiteralValue(bits: Bits): (Bits, Bits) = {
      val (prefixBit :: groupBits, bits2) = bits.splitAt(5)
      if (prefixBit) {
        val (tailBits, bits3) = parseLiteralValue(bits2)
        (groupBits ++ tailBits, bits3)
      } else
        (groupBits, bits2)
    }

    def parseSubpacketsLength(bits: Bits): List[Packet] = bits match {
      case Nil =>
        Nil
      case _ =>
        val (packet, bits2) = parsePacket(bits)
        val packets = parseSubpacketsLength(bits2)
        packet :: packets
    }

    def parseSubpacketsNumber(bits: Bits, n: Int): (List[Packet], Bits) = {
      if (n == 0)
        (Nil, bits)
      else {
        val (packet, bits2) = parsePacket(bits)
        val (packets, bits3) = parseSubpacketsNumber(bits2, n - 1)
        (packet :: packets, bits3)
      }
    }

    def parsePacket(bits: Bits): (Packet, Bits) = {
      val (versionBits, bits2) = bits.splitAt(3)
      val version = bits2int(versionBits).toInt
      val (typeIdBits, bits3) = bits2.splitAt(3)
      val typeId = bits2int(typeIdBits).toInt
      typeId match {
        case 4 =>
          val (valueBits, bits4) = parseLiteralValue(bits3)
          val value = bits2int(valueBits)
          (Literal(version, value), bits4)
        case _ =>
          val lengthTypeBit :: bits4 = bits3
          if (lengthTypeBit) {
            val (numberBits, bits5) = bits4.splitAt(11)
            val number = bits2int(numberBits).toInt
            val (subpackets, bits6) = parseSubpacketsNumber(bits5, number)
            (Operator(version, typeId, subpackets), bits6)
          }
          else {
            val (lengthBits, bits5) = bits4.splitAt(15)
            val length = bits2int(lengthBits).toInt
            val (subpacketBits, bits6) = bits5.splitAt(length)
            val subpackets = parseSubpacketsLength(subpacketBits)
            (Operator(version, typeId, subpackets), bits6)
          }
      }
    }

    override def parse(bits: Bits): Packet = parsePacket(bits)._1
  }

  object ParserCombinatorSolution extends Solution with ListParsers[Boolean] with ExtraParsers {

    def long(n: Int): Parser[Long] = repN(n, any) ^^ bits2int
    def int(n: Int): Parser[Int] = long(n) ^^ (_.toInt)

    def literalValue: Parser[Long] = rep(true ~> long(4)) ~ (false ~> long(4)) ^^ {
      case xs ~ x => (xs.foldLeft(0L)((acc, x) => (acc << 4) | x) << 4) | x
    }

    def subpackets: Parser[List[Packet]] = (
      true ~> int(11) >> (repN(_, packet))
    | false ~> int(15) >> (lengthed(_, rep(packet)))
    )

    def packet: Parser[Packet] = int(3) ~ int(3) >> {
      case version ~ 4 =>
        literalValue ^^ (Literal(version, _))
      case version ~ typeId =>
        subpackets ^^ (Operator(version, typeId, _))
    }

    override def parse(bits: Bits): Packet = parse(packet, bits).get
  }


  def sumVersions(packet: Packet): Int = packet match {
    case Literal(version, _) =>
      version
    case Operator(version, _, subpackets) =>
      version + subpackets.map(sumVersions).sum
  }

  def eval(packet: Packet): Long = packet match {
    case Literal(_, value) =>
      value
    case Operator(_, typeId, subpackets) =>
      val subvalues = subpackets.map(eval)
      typeId match {
        case 0 => subvalues.sum
        case 1 => subvalues.product
        case 2 => subvalues.min
        case 3 => subvalues.max
        case 5 =>
          val Seq(value1, value2) = subvalues
          if (value1 > value2)
            1
          else
            0
        case 6 =>
          val Seq(value1, value2) = subvalues
          if (value1 < value2)
            1
          else
            0
        case 7 =>
          val Seq(value1, value2) = subvalues
          if (value1 == value2)
            1
          else
            0
      }
  }


  def parseBinBits(input: String): Bits = input.view.map({
    case '0' => false
    case '1' => true
  }).toList

  private val hexBits = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111",
  ).view.mapValues(parseBinBits).toMap

  def parseHexBits(input: String): Bits = input.view.flatMap(hexBits).toList


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import ParserCombinatorSolution._

    println(sumVersions(parseHex(input)))
    println(eval(parseHex(input)))

    // part 2: 2904570676 - wrong (need Long for Literal values and eval)
  }
}
