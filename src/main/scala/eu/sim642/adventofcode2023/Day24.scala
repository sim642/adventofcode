package eu.sim642.adventofcode2023

import com.microsoft.z3.{Context, Status}
import eu.sim642.adventofcodelib.pos.Pos3

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters.*

object Day24 {

  case class LongPos3(x: Long, y: Long, z: Long)

  case class Hailstone(pos: LongPos3, velocity: Pos3)

  // copied from 2021 day 5
  def intersection1(hailstone1: Hailstone, hailstone2: Hailstone): Option[(Double, Double)] = {
    val LongPos3(x1, y1, _) = hailstone1.pos
    val x2 = x1 + hailstone1.velocity.x
    val y2 = y1 + hailstone1.velocity.y
    val LongPos3(x3, y3, _) = hailstone2.pos
    val x4 = x3 + hailstone2.velocity.x
    val y4 = y3 + hailstone2.velocity.y
    // https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection#Given_two_points_on_each_line_segment
    val d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    val t0 = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)
    val u0 = (x1 - x3) * (y1 - y2) - (y1 - y3) * (x1 - x2)
    if (d == 0) { // parallel
      if (t0 == 0 && u0 == 0) { // overlapping
        ???
      }
      else // not overlapping
        None
    }
    //else if ((0 <= t0 && t0 <= d || 0 <= -t0 && -t0 <= -d) && (0 <= u0 && u0 <= d || 0 <= -u0 && -u0 <= -d)) {
    else if ((0 <= t0 && 0 <= d || 0 <= -t0 && 0 <= -d) && (0 <= u0 && 0 <= d || 0 <= -u0 && 0 <= -d)) {
      val t = t0 / d.toDouble
      val x = x1 + t * (x2 - x1)
      val y = y1 + t * (y2 - y1)
      Some((x, y))
    }
    else
      None
  }

  def countIntersections1(hailstones: Seq[Hailstone], minCoord: Long = 200000000000000L, maxCoord: Long = 400000000000000L): Int = {
    // copied & modified from 2021 day 5
    val filteredLines = hailstones.to(ArraySeq) // ArraySeq for view slicing

    val inters: Iterator[(Double, Double)] =
      for {
        // faster than combinations(2)
        (hailstone1, i) <- filteredLines.iterator.zipWithIndex
        hailstone2 <- filteredLines.view.slice(i + 1, filteredLines.size).iterator
        p@(px, py) <- intersection1(hailstone1, hailstone2).iterator
        if px >= minCoord && px <= maxCoord
        if py >= minCoord && py <= maxCoord
      } yield p

    inters.size
  }

  def sumThrowCoords(hailstones: Seq[Hailstone]): Long = {
    val ctx = new Context(Map("model" -> "true").asJava)
    import ctx.*
    val s = mkSolver()

    // 6 unknowns
    val x = mkRealConst("x")
    val y = mkRealConst("y")
    val z = mkRealConst("z")
    val vx = mkRealConst("vx")
    val vy = mkRealConst("vy")
    val vz = mkRealConst("vz")

    for (i <- 0 until 3) { // 9 equations, 9 unknowns
      // 3 equations, 1 unknown
      val Hailstone(p, v) = hailstones(i)
      val ti = ctx.mkRealConst(s"t${i}")
      s.add(mkEq(mkAdd(mkReal(p.x), mkMul(ti, mkReal(v.x))), mkAdd(x, mkMul(ti, vx))))
      s.add(mkEq(mkAdd(mkReal(p.y), mkMul(ti, mkReal(v.y))), mkAdd(y, mkMul(ti, vy))))
      s.add(mkEq(mkAdd(mkReal(p.z), mkMul(ti, mkReal(v.z))), mkAdd(z, mkMul(ti, vz))))
    }

    assert(s.check() == Status.SATISFIABLE)
    s.getModel.evaluate(mkAdd(x, mkAdd(y, z)), false).toString.toLong
  }


  def parseHailstone(s: String): Hailstone = s match {
    case s"$x, $y, $z @ $vx, $vy, $vz" =>
      val pos = LongPos3(x.trim.toLong, y.trim.toLong, z.trim.toLong)
      val velocity = Pos3(vx.trim.toInt, vy.trim.toInt, vz.trim.toInt)
      Hailstone(pos, velocity)
  }

  def parseHailstones(input: String): Seq[Hailstone] = input.linesIterator.map(parseHailstone).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countIntersections1(parseHailstones(input)))
    println(sumThrowCoords(parseHailstones(input)))
  }
}
