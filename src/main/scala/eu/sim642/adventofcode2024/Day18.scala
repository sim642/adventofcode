package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.OrderedSearch
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, TargetNode, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day18 {

  def bytesGraphSearch(bytes: Seq[Pos], max: Pos = Pos(70, 70), after: Int = 1024): GraphSearch[Pos] & UnitNeighbors[Pos] & TargetNode[Pos] = {
    val fallenBytes = bytes.take(after).toSet

    new GraphSearch[Pos] with UnitNeighbors[Pos] with TargetNode[Pos] {
      override val startNode: Pos = Pos.zero

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if Pos.zero <= newPos && newPos <= max
          if !fallenBytes(newPos)
        } yield newPos
      }

      override val targetNode: Pos = max
    }
  }

  def exitSteps(bytes: Seq[Pos], max: Pos = Pos(70, 70), after: Int = 1024): Int = {
    val graphSearch = bytesGraphSearch(bytes, max, after)
    BFS.search(graphSearch).target.get._2
  }

  trait Part2Solution {
    def findBlockingByte(bytes: Seq[Pos], max: Pos = Pos(70, 70)): Pos

    def findBlockingByteString(bytes: Seq[Pos], max: Pos = Pos(70, 70)): String = {
      val blockingByte = findBlockingByte(bytes, max)
      s"${blockingByte.x},${blockingByte.y}"
    }
  }

  object BinarySearchPart2Solution extends Part2Solution {
    def exitReachable(bytes: Seq[Pos], max: Pos, after: Int): Boolean = {
      val graphSearch = bytesGraphSearch(bytes, max, after)
      BFS.search(graphSearch).target.isDefined
    }

    override def findBlockingByte(bytes: Seq[Pos], max: Pos = Pos(70, 70)): Pos = {
      def f(after: Int): Boolean = !exitReachable(bytes, max, after)
      val blockingAfter = OrderedSearch.binaryLower(f, 0, bytes.size + 1)(true)
      bytes(blockingAfter - 1)
    }
  }

  object LinearOnPathPart2Solution extends Part2Solution {
    def exitPath(bytes: Seq[Pos], max: Pos, after: Int): Option[Seq[Pos]] = {
      val graphSearch = bytesGraphSearch(bytes, max, after + 1)
      BFS.searchPaths(graphSearch).paths.lift(graphSearch.targetNode)
    }

    override def findBlockingByte(bytes: Seq[Pos], max: Pos = Pos(70, 70)): Pos = {

      @tailrec
      def helper(after: Int, path: Set[Pos]): Int = {
        if (path(bytes(after))) {
          exitPath(bytes, max, after + 1) match {
            case Some(newPath) => helper(after + 1, newPath.toSet)
            case None => after + 1
          }
        }
        else
          helper(after + 1, path)
      }

      val blockingAfter = helper(0, exitPath(bytes, max, 0).get.toSet)
      bytes(blockingAfter - 1)
    }
  }

  def parseByte(s: String): Pos = s match {
    case s"$x,$y" => Pos(x.toInt, y.toInt)
  }

  def parseBytes(input: String): Seq[Pos] = input.linesIterator.map(parseByte).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import BinarySearchPart2Solution._
    println(exitSteps(parseBytes(input)))
    println(findBlockingByteString(parseBytes(input)))
  }
}
