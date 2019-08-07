package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2017.Day3.Pos

object Day22 {

  case class Disk(used: Int, available: Int)

  def countViablePairs(nodes: Map[Pos, Disk]): Int = {
    val viablePairs = for {
      a@(aPos, aDisk) <- nodes
      if aDisk.used > 0
      b@(bPos, bDisk) <- nodes
      if bPos != aPos
      if aDisk.used <= bDisk.available
    } yield (a, b)

    viablePairs.size
  }

  private val nodeRegex = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r

  val parseNode: PartialFunction[String, (Pos, Disk)] = {
    case nodeRegex(x, y, _, used, avail, _) =>
      Pos(x.toInt, y.toInt) -> Disk(used.toInt, avail.toInt)
  }

  def parseNodes(input: String): Map[Pos, Disk] = input.lines.collect(parseNode).toMap

  def countViablePairs(input: String): Int = countViablePairs(parseNodes(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countViablePairs(input))
  }
}
