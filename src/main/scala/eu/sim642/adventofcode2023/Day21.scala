package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{GraphSearch, SimultaneousBFS, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits._
import Integral.Implicits._

object Day21 {

  def countReachableExactly(grid: Grid[Char], steps: Int = 64): Int = {

    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = grid.posOf('S')

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if grid.containsPos(newPos)
          if grid(newPos) != '#'
        } yield newPos
      }

      override def isTargetNode(pos: Pos, dist: Int): Boolean = dist == steps
    }

    SimultaneousBFS.search(graphSearch).distances.count(_._2 % 2 == steps % 2)
  }

  trait Part2Solution {
    def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Long
  }

  object NaivePart2Solution extends Part2Solution {

    // copied from 2021 day 25
    extension (pos: Pos) {
      def %+(other: Pos): Pos = Pos(pos.x %+ other.x, pos.y %+ other.y)
    }

    override def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Long = {
      val gridSize = Pos(grid(0).size, grid.size)

      val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] {
        override val startNode: Pos = grid.posOf('S')

        override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
          for {
            offset <- Pos.axisOffsets
            newPos = pos + offset
            if grid(newPos %+ gridSize) != '#'
          } yield newPos
        }

        override def isTargetNode(pos: Pos, dist: Int): Boolean = dist == steps
      }

      SimultaneousBFS.search(graphSearch).distances.count(_._2 % 2 == steps % 2)
    }
  }

  object QuadraticPart2Solution extends Part2Solution {

    override def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Long = {
      val (q, r) = steps /% 131
      //println((q, r))

      val x1 = 0
      val y0 = NaivePart2Solution.countReachableExactlyInfinite(grid, r)
      val x2 = 1
      val y1 = NaivePart2Solution.countReachableExactlyInfinite(grid, r + 131)
      val x3 = 2
      val y2 = NaivePart2Solution.countReachableExactlyInfinite(grid, r + 2 * 131)

      //def f(x: Long): Long = {
      //  y0 * (x - x2) * (x - x3) / (x1 - x2) / (x1 - x3) +
      //    y1 * (x - x1) * (x - x3) / (x2 - x1) / (x2 - x3) +
      //    y2 * (x - x1) * (x - x2) / (x3 - x1) / (x3 - x2)
      //}

      def f(x: Long): Long = {
        y0 * (x - 1) * (x - 2) / 2 - y1 * x * (x - 2) + y2 * x * (x - 1) / 2
      }
      /*

      /\
      \/


       */

      println(y0)
      println(y1)
      println(y2)

      // y0 0 (1) [1, 0, 0] = innerEven
      // y1 1 (9) [1, 4, 4] = innerEven + 4 * outer + 4 * innerOdd
      // y2 2 (25) [9, 12, 4] = innerEven + 4 * outer + 4 * innerOdd + 8 * outer + 8 * innerEven

      // y4 3 (49) [9, 24, 16] = innerEven + 4 * outer + 4 * innerOdd + 8 * outer + 8 * innerEven + 12 * outer + 12 * innerOdd


      /*
      f(0) = 1A
       AA
      AAAA
      AAAA
       AA

      f(1) = 1A + 4B + 2X + 2Y
             BB
            BBBB
            BBBB
          XY BB YX
         XXYY  YYXX
         YYXX  XXYY
       BB YX AA XY BB
      BBBB  AAAA  BBBB
      BBBB  AAAA  BBBB
       BB YX AA XY BB
         YYXX  XXYY
         XXYY  YYXX
          XY BB YX
            BBBB
            BBBB
             BB

    f(2) = 9A + 4B + 6X + 6Y
             AA
            AAAA
            AAAA
          YX AA XY
         YYXX  XXYY
         XXYY  YYXX
       AA XY BB YX AA
      AAAA  BBBB  AAAA
      AAAA  BBBB  AAAA
    YX AA XY BB YX AA XY
   YYXX  XXYY  YYXX  XXYY
   XXYY  YYXX  XXYY  YYXX
 AA XY BB YX AA XY BB YX AA
AAAA  BBBB  AAAA  BBBB  AAAA
AAAA  BBBB  AAAA  BBBB  AAAA
 AA XY BB YX AA XY BB YX AA
   XXYY  YYXX  XXYY  YYXX
   YYXX  XXYY  YYXX  XXYY
    YX AA XY BB YX AA XY
      AAAA  BBBB  AAAA
      AAAA  BBBB  AAAA
       AA XY BB YX AA
         XXYY  YYXX
         YYXX  XXYY
          YX AA XY
            AAAA
            AAAA
             AA

      f(3) = 9A + 16B +
      */


      def f2(n: Long): Long = {
        val a = y0 // A
        val xy = (y2 - y1 - 8 * a).toDouble / 4 // X + Y
        val b = (y1 - y0 - 2 * xy) / 4 // B
        //val xy = (-8 * y0 - y1 + y2) / 4
        //val b = (6 * y0 + 3 * y1 - y2) / 8
        println((a, b, xy))

        val c = (2 * n + 1) * (2 * n + 1)
        // n:  0, 1, 2,  3, ...
        // ca: 1, 1, 9,  9, ...
        // cb: 0, 4, 4, 16, ...
        val ca = (2 * (n / 2) + 1) * (2 * (n / 2) + 1)
        //val cb = inners - ca
        val cb = (2 * ((n + 1) / 2)) * (2 * ((n + 1) / 2))
        val cxy = (c - ca - cb) / 2
        val r = ca * a + cb * b + cxy * xy
        println((n, r))
        println((ca, cb, cxy))
        r.toLong
      }

      //assert(f2(0) == y0)
      //assert(f2(1) == y1)
      //assert(f2(2) == y2)
      f2(q)
    }
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import QuadraticPart2Solution._

    println(countReachableExactly(parseGrid(input)))
    println(countReachableExactlyInfinite(parseGrid(input)))
  }
}
