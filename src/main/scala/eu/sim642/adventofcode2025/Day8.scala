package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.graph.Kruskal
import eu.sim642.adventofcodelib.pos.Pos3

import scala.collection.mutable

object Day8 {

  extension (pos: Pos3) {
    infix def euclideanDistanceSqr(that: Pos3): Long = {
      val d = that - pos
      d.x.toLong * d.x + d.y.toLong * d.y + d.z.toLong * d.z
    }
  }

  extension [A](queue: mutable.PriorityQueue[A]) {
    // normal queue.iterator does not yield dequeue order
    def dequeueIterator: Iterator[A] = new Iterator[A] {
      override def hasNext: Boolean = queue.nonEmpty

      override def next(): A = queue.dequeue()
    }
  }

  def iterateClosestPairs(junctionBoxes: Seq[Pos3]): Iterator[(Pos3, Pos3)] = {
    // it is faster to use a PriorityQueue than sort the Seq of all pairs because Kruskal will only need some closest pairs, not all
    val queue = mutable.PriorityQueue.empty[((Pos3, Pos3), Long)](using Ordering.by(-_._2))
    for {
      // faster than combinations(2)
      (p1, i) <- junctionBoxes.iterator.zipWithIndex
      p2 <- junctionBoxes.view.slice(i + 1, junctionBoxes.size).iterator
    } queue.enqueue((p1, p2) -> (p1 euclideanDistanceSqr p2)) // no need to sqrt distance just for sorting
    queue.dequeueIterator.map(_._1)
  }

  def multiplySizesAfter(junctionBoxes: Seq[Pos3], after: Int = 1000, sizes: Int = 3): Int = {
    val closestPairs = iterateClosestPairs(junctionBoxes)
    val (ufAfter, _) = Kruskal.iterate(junctionBoxes, closestPairs)(after)

    ufAfter.groups()
      .map(_.size)
      .sorted(using Ordering.Int.reverse)
      .take(sizes)
      .product
  }

  def multiplyLastXs(junctionBoxes: Seq[Pos3]): Int = {
    val closestPairs = iterateClosestPairs(junctionBoxes)
    val (p1, p2) = Kruskal.iterateEdges(junctionBoxes, closestPairs).last
    p1.x * p2.x
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
