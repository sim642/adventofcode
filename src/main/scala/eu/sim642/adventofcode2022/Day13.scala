package eu.sim642.adventofcode2022

import Day13.PacketNode.*

import scala.util.parsing.combinator.*
import math.Ordering.Implicits._
import scala.annotation.tailrec

object Day13 extends RegexParsers {

  enum PacketNode {
    case IntNode(value: Int)
    case ListNode(nodes: Seq[PacketNode])
  }

  //noinspection ConvertExpressionToSAM
  given Ordering[PacketNode] = new Ordering[PacketNode] {

    import math.Ordered.orderingToOrdered

    @tailrec
    override def compare(x: PacketNode, y: PacketNode): Int = (x, y) match {
      case (IntNode(value1), IntNode(value2)) => value1.compare(value2)
      case (ListNode(nodes1), ListNode(nodes2)) => nodes1.compare(nodes2)
      case (IntNode(_), ListNode(_)) => compare(ListNode(Seq(x)), y)
      case (ListNode(_), IntNode(_)) => compare(x, ListNode(Seq(y)))
    }
  }

  def sumPairOrderedIndices(packets: Seq[PacketNode]): Int = {
    (for {
      (Seq(packet1, packet2), i) <- packets.grouped(2).zipWithIndex
      if packet1 <= packet2
    } yield i + 1).sum
  }

  private val divider2 = ListNode(Seq(ListNode(Seq(IntNode(2)))))
  private val divider6 = ListNode(Seq(ListNode(Seq(IntNode(6)))))

  def decoderKey(packets: Seq[PacketNode]): Int = {
    val allPackets = packets ++ Seq(divider2, divider6)
    val sortedPackets = allPackets.sorted
    (sortedPackets.indexOf(divider2) + 1) * (sortedPackets.indexOf(divider6) + 1)
  }


  def parsePacket(s: String): PacketNode = {

    def packetNode: Parser[PacketNode] = (
      "\\d+".r ^^ (value => IntNode(value.toInt))
    | "[" ~> repsep(packetNode, ",") <~ "]" ^^ ListNode.apply
    )

    parseAll(packetNode, s).get
  }

  def parsePackets(input: String): Seq[PacketNode] = input.linesIterator.filterNot(_.isEmpty).map(parsePacket).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumPairOrderedIndices(parsePackets(input)))
    println(decoderKey(parsePackets(input)))
  }
}
