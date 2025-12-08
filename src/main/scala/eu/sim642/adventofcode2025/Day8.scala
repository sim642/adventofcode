package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.pos.Pos3
import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.UnionFind

import scala.annotation.tailrec

object Day8 {

  extension (pos: Pos3) {
    infix def euclideanDistance(that: Pos3): Double = {
      val d = that - pos
      math.sqrt(d.x.toDouble * d.x + d.y.toDouble * d.y + d.z.toDouble * d.z)
    }
  }

  def closestPairsSeq(junctionBoxes: Seq[Pos3]): Seq[(Pos3, Pos3)] = {
    //noinspection ConvertibleToMethodValue
    (for {
      // faster than combinations(2)
      (p1, i) <- junctionBoxes.iterator.zipWithIndex
      p2 <- junctionBoxes.view.slice(i + 1, junctionBoxes.size).iterator
    } yield (p1, p2) -> (p1 euclideanDistance p2))
      .toSeq
      .sortBy(_._2)
      .map(_._1)
  }

  def multiplySizesAfter(junctionBoxes: Seq[Pos3], after: Int = 1000, sizes: Int = 3): Int = {
    val closestPairs = closestPairsSeq(junctionBoxes)

    val ufAfter = closestPairs.iterator
      .scanLeft(new UnionFind(junctionBoxes))({ case (uf, (p1, p2)) =>
        uf.unioned(p1, p2)
      })(after)

    ufAfter.groups()
      .map(_.size)
      .sorted(using Ordering.Int.reverse)
      .take(sizes)
      .product
  }

  // TODO: deduplicate
  def multiplyLastXs(junctionBoxes: Seq[Pos3]): Int = {
    val closestPairs = closestPairsSeq(junctionBoxes)

    // TODO: clean up
    val size = junctionBoxes.size
    val ufAfter = closestPairs.iterator
      .scanLeft((new UnionFind(junctionBoxes), 0))({ case ((uf, edges), (p1, p2)) =>
        if (uf.sameRepr(p1, p2))
          (uf, edges)
        else
          (uf.unioned(p1, p2), edges + 1)
      }).tail.zip(closestPairs).find(_._1._2 == size - 1).get._2

    ufAfter._1.x * ufAfter._2.x
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
