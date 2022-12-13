package eu.sim642.adventofcode2022

import Day13.PacketNode.*

import scala.util.parsing.combinator.*
import math.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec

object Day13 extends RegexParsers {

  enum PacketNode {
    case IntNode(value: Int)
    case ListNode(nodes: Seq[PacketNode])
  }

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

  def sumOrderedIndices(packetPairs: Seq[(PacketNode, PacketNode)]): Int = {
    (for {
      ((packet1, packet2), i) <- packetPairs.zipWithIndex
      if packet1 <= packet2
    } yield i + 1).sum
  }

  def parsePacket(s: String): PacketNode = {

    def packetNode: Parser[PacketNode] = (
      "\\d+".r ^^ (value => IntNode(value.toInt))
    | "[" ~> repsep(packetNode, ",") <~ "]" ^^ ListNode.apply
    )

    parseAll(packetNode, s).get
  }

  def parsePacketPairs(input: String): Seq[(PacketNode, PacketNode)] = {
    input.split("\n\n").map({ pair =>
      val Array(packet1, packet2) = pair.split("\n", 2)
      (parsePacket(packet1), parsePacket(packet2))
    }).toSeq
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumOrderedIndices(parsePacketPairs(input)))
  }
}
