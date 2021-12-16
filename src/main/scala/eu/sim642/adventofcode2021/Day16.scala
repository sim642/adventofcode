package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{Dijkstra, GraphSearch, TargetNode}
import eu.sim642.adventofcodelib.pos.Pos

object Day16 {

  type Bits = List[Boolean]

  def bits2int(bits: Bits): Long = {
    bits.foldLeft(0L)({ case (acc, bit) => (acc << 1) | (if bit then 1 else 0) })
  }


  sealed trait Packet {
    val version: Int
  }
  case class Literal(version: Int, value: Long) extends Packet
  case class Operator(version: Int, typeId: Int, subpackets: List[Packet]) extends Packet


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

  // TODO: rename
  def parsePacket2(bits: Bits): Packet = parsePacket(bits)._1


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


  private val hexBits = Map(
    '0' -> List(0, 0, 0, 0),
    '1' -> List(0, 0, 0, 1),
    '2' -> List(0, 0, 1, 0),
    '3' -> List(0, 0, 1, 1),
    '4' -> List(0, 1, 0, 0),
    '5' -> List(0, 1, 0, 1),
    '6' -> List(0, 1, 1, 0),
    '7' -> List(0, 1, 1, 1),
    '8' -> List(1, 0, 0, 0),
    '9' -> List(1, 0, 0, 1),
    'A' -> List(1, 0, 1, 0),
    'B' -> List(1, 0, 1, 1),
    'C' -> List(1, 1, 0, 0),
    'D' -> List(1, 1, 0, 1),
    'E' -> List(1, 1, 1, 0),
    'F' -> List(1, 1, 1, 1),
  ).view.mapValues(_.map({ case 0 => false case 1 => true}))

  def parseHexBits(input: String): Bits = input.view.flatMap(hexBits).toList

  // for testing
  def parseBinBits(input: String): Bits = input.view.map({ case '0' => false case '1' => true}).toList

  def parseHexPacket(input: String): Packet = parsePacket2(parseHexBits(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumVersions(parseHexPacket(input)))
    println(eval(parseHexPacket(input)))

    // part 2: 2904570676 - wrong (need Long for Literal values and eval)
  }
}
