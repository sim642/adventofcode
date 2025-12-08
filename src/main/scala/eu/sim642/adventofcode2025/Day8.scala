package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.graph.Kruskal
import eu.sim642.adventofcodelib.pos.Pos3

object Day8 {

  extension (pos: Pos3) {
    infix def euclideanDistanceSqr(that: Pos3): Long = {
      val d = that - pos
      d.x.toLong * d.x + d.y.toLong * d.y + d.z.toLong * d.z
    }
  }

  def closestPairsSeq(junctionBoxes: Seq[Pos3]): Seq[(Pos3, Pos3)] = {
    //noinspection ConvertibleToMethodValue
    (for {
      // faster than combinations(2)
      (p1, i) <- junctionBoxes.iterator.zipWithIndex
      p2 <- junctionBoxes.view.slice(i + 1, junctionBoxes.size).iterator
    } yield (p1, p2) -> (p1 euclideanDistanceSqr p2)) // no need to sqrt distance just for sorting
      .toSeq
      .sortBy(_._2)
      .map(_._1)
  }

  def multiplySizesAfter(junctionBoxes: Seq[Pos3], after: Int = 1000, sizes: Int = 3): Int = {
    val closestPairs = closestPairsSeq(junctionBoxes)
    val (ufAfter, _) = Kruskal.iterate(junctionBoxes, closestPairs)(after)

    ufAfter.groups()
      .map(_.size)
      .sorted(using Ordering.Int.reverse)
      .take(sizes)
      .product
  }

  // TODO: deduplicate
  def multiplyLastXs(junctionBoxes: Seq[Pos3]): Int = {
    val closestPairs = closestPairsSeq(junctionBoxes)
    val lastPair = Kruskal.iterateEdges(junctionBoxes, closestPairs).last
    lastPair._1.x * lastPair._2.x
  }

  def parseJunctionBox(s: String): Pos3 = s match {
    case s"$x,$y,$z" => Pos3(x.toInt, y.toInt, z.toInt)
  }

  def parseJunctionBoxes(input: String): Seq[Pos3] = input.linesIterator.map(parseJunctionBox).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(multiplySizesAfter(parseJunctionBoxes(input)))
    println(multiplyLastXs(parseJunctionBoxes(input)))
  }
}
