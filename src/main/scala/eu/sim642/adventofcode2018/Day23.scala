package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day20.Pos3
import eu.sim642.adventofcode2018.Day25.Pos4

import scala.collection.mutable
import scala.util.control.Breaks._

object Day23 {

  case class Nanobot(pos: Pos3, radius: Int) {
    def overlaps(that: Nanobot): Boolean = (this.pos manhattanDistance that.pos) <= this.radius + that.radius

    def contains(cPos: Pos3): Boolean = (pos manhattanDistance cPos) <= radius

    lazy val corners: Seq[Pos3] = {
      Seq(
        Pos3(-radius, 0, 0),
        Pos3(radius, 0, 0),
        Pos3(0, -radius, 0),
        Pos3(0, radius, 0),
        Pos3(0, 0, -radius),
        Pos3(0, 0, radius),
      ).map(pos + _)
    }

    def contains(that: Nanobot): Boolean = that.corners.forall(contains)
  }

  def nanobotsInLargestRadius(nanobots: Seq[Nanobot]): Int = {
    val largestRadius = nanobots.maxBy(_.radius)
    nanobots.count(nanobot => largestRadius.contains(nanobot.pos))
  }

  trait Part2Solution {
    def closestMostNanobots(nanobots: Seq[Nanobot]): Int
  }

  object NaiveCliquePart2Solution extends Part2Solution {
    def maximumClique(neighbors: Map[Nanobot, Set[Nanobot]]): Set[Nanobot] = {
      var best: Set[Nanobot] = Set.empty

      def bronKerbosh(r: Set[Nanobot], p: Set[Nanobot], x: Set[Nanobot]): Unit = {
        if (p.isEmpty && x.isEmpty) {
          //println(r)
          if (r.size > best.size)
            best = r
        }
        else {
          //val u = p.headOption.getOrElse(x.head)
          val u = (p ++ x).maxBy(neighbors(_).size) // pivot on highest degree
          var p2 = p
          var x2 = x
          for (v <- p -- neighbors(u)) {
            bronKerbosh(r + v, p2 intersect neighbors(v), x2 intersect neighbors(v))
            p2 -= v
            x2 += v
          }
        }
      }

      bronKerbosh(Set.empty, neighbors.keySet, Set.empty)
      best
    }

    def maximumOverlap(nanobots: Seq[Nanobot]): Set[Nanobot] = {
      val neighbors: Map[Nanobot, Set[Nanobot]] = nanobots.map(nanobot1 => nanobot1 -> nanobots.filter(nanobot2 => nanobot2 != nanobot1 && nanobot1.overlaps(nanobot2)).toSet).toMap
      maximumClique(neighbors)
    }

    def closestMostNanobots(nanobots: Seq[Nanobot]): Int = {
      maximumOverlap(nanobots).map(n => (n.pos manhattanDistance Pos3(0, 0, 0)) - n.radius).max
    }
  }

  object FourDimCliquePart2Solution extends Part2Solution {

    implicit class Pos4Ops(pos4: Pos4) {
      def +(that: Pos4): Pos4 = Pos4(pos4.x + that.x, pos4.y + that.y, pos4.z + that.z, pos4.w + that.w)
    }

    implicit class ExactDivideInt(n: Int) {
      def /!(d: Int): Option[Int] = if (n % d == 0) Some(n / d) else None
    }

    case class Box4(min: Pos4, max: Pos4) {
      def intersect(that: Box4): Option[Box4] = {
        val Box4(min2, max2) = that
        val minX = min.x max min2.x
        val maxX = max.x min max2.x
        val minY = min.y max min2.y
        val maxY = max.y min max2.y
        val minZ = min.z max min2.z
        val maxZ = max.z min max2.z
        val minW = min.w max min2.w
        val maxW = max.w min max2.w
        if (minX <= maxX && minY <= maxY && minZ <= maxZ && minW <= maxW)
          Some(Box4(Pos4(minX, minY, minZ, minW), Pos4(maxX, maxY, maxZ, maxW)))
        else
          None
      }
    }

    def nanobot2box4(nanobot: Nanobot): Box4 = {
      val Nanobot(Pos3(x, y, z), r) = nanobot
      val x_ = x + y + z
      val y_ = x + y - z
      val z_ = x - y - z
      val w_ = x - y + z
      val center = Pos4(x_, y_, z_, w_)
      Box4(center + Pos4(-r, -r, -r, -r), center + Pos4(r, r, r, r))
    }

    def pos42pos3(pos4: Pos4): Option[Pos3] = {
      val Pos4(x_, y_, z_, w_) = pos4
      for {
        x <- (x_ + z_) /! 2
        y <- (y_ - z_) /! 2
        z <- (x_ - y_) /! 2
        if x - y + z == w_
      } yield Pos3(x, y, z)
    }

    def box4iterate(box4: Box4): Iterator[Pos4] = {
      val Box4(Pos4(x1, y1, z1, w1), Pos4(x2, y2, z2, w2)) = box4
      for {
        x <- (x1 to x2).toIterator
        y <- (y1 to y2).toIterator
        z <- (z1 to z2).toIterator
        w <- (w1 to w2).toIterator
      } yield Pos4(x, y, z, w)
    }

    override def closestMostNanobots(nanobots: Seq[Nanobot]): Int = {
      val overlapping = NaiveCliquePart2Solution.maximumOverlap(nanobots)
      val intersection = overlapping.map(nanobot => Option(nanobot2box4(nanobot))).reduce({ (left, right) =>
        for {
          l <- left
          r <- right
          i <- l intersect r
        } yield i
      }).get
      //println(intersection)

      val pos4s = box4iterate(intersection).toSeq
      //println(pos4s.size)
      val pos3s = pos4s.flatMap(pos42pos3)
      //println(pos3s.size)
      pos3s.map(p => p manhattanDistance Pos3(0, 0, 0)).min
    }
  }

  trait SplittingPart2Solution extends Part2Solution {
    type A

    def getInitial(nanobots: Seq[Nanobot]): A

    def nanobotContains(nanobot: Nanobot, a: A): Boolean

    def nanobotOverlaps(nanobot: Nanobot, a: A): Boolean

    def getBounds(nanobots: Seq[Nanobot], a: A): (Int, Int) = {
      val lower = nanobots.count(nanobotContains(_, a))
      val upper = nanobots.count(nanobotOverlaps(_, a))
      (lower, upper)
    }

    def getSplits(a: A): Set[A]

    def originDistance(a: A): Int
  }

  trait GreedySplittingPart2Solution extends SplittingPart2Solution {
    override def closestMostNanobots(nanobots: Seq[Nanobot]): Int = {
      val queue: mutable.PriorityQueue[(A, (Int, Int), Int)] =
        mutable.PriorityQueue.empty(Ordering.by({ case (octahedron, (lower, upper), originDist) =>
          (upper, lower, -originDist)
          //(upper, -a.radius, -originDist) // much faster but possibly incorrect?
        }))
      val done: mutable.Set[A] = mutable.Set.empty

      def enqueue(a: A): Unit = {
        queue.enqueue((a, getBounds(nanobots, a), originDistance(a)))
      }

      val initial = getInitial(nanobots)
      enqueue(initial)

      breakable {
        while (queue.nonEmpty) {
          val (a, (lower, upper), originDist) = queue.dequeue()
          if (!done.contains(a)) {
            done += a

            if (lower == upper)
              return originDist

            for (splitOctahedron <- getSplits(a))
              enqueue(splitOctahedron)
          }
        }
      }
      ???
    }
  }

  trait NonGreedySplittingPart2Solution extends SplittingPart2Solution {
    override def closestMostNanobots(nanobots: Seq[Nanobot]): Int = {
      val queue: mutable.PriorityQueue[(A, (Int, Int), Int)] =
        mutable.PriorityQueue.empty(Ordering.by({ case (octahedron, (lower, upper), originDist) =>
          (upper, lower, -originDist)
          //(upper, -a.radius, -originDist) // much faster but possibly incorrect?
        }))
      val done: mutable.Set[A] = mutable.Set.empty

      def enqueue(a: A): Unit = {
        queue.enqueue((a, getBounds(nanobots, a), originDistance(a)))
      }

      val initial = getInitial(nanobots)
      enqueue(initial)

      var closest: Option[(A, Int, Int)] = None
      breakable {
        while (queue.nonEmpty) {
          val (a, (lower, upper), originDist) = queue.dequeue()
          if (!done.contains(a)) {
            done += a

            if (closest.exists(_._2 > upper))
              break()
            else if (lower == upper) {
              closest match {
                case None =>
                  closest = Some((a, upper, originDist))
                //break()
                case Some((_, closestUpper, closestOriginDist)) =>
                  if (originDist < closestOriginDist)
                    closest = Some((a, upper, originDist))
              }
            }

            for (splitBox <- getSplits(a))
              enqueue(splitBox)
          }
          //else
          //  println(s"dup: $box")
        }
      }
      closest.get._3
    }
  }

  object OctahedronSplittingPart2Solution extends GreedySplittingPart2Solution {
    override type A = Nanobot

    override def getInitial(nanobots: Seq[Nanobot]): Nanobot = {
      //val initPos = Pos3(0, 0, 0)
      val poss = nanobots.map(_.pos)
      val initX = (poss.map(_.x).min + poss.map(_.x).max) / 2
      val initY = (poss.map(_.y).min + poss.map(_.y).max) / 2
      val initZ = (poss.map(_.z).min + poss.map(_.z).max) / 2
      val initPos = Pos3(initX, initY, initZ)
      val radius = nanobots.map(nanobot => (initPos manhattanDistance nanobot.pos) + nanobot.radius).max
      Nanobot(initPos, radius)
    }

    override def nanobotContains(nanobot: Nanobot, octahedron: Nanobot): Boolean = nanobot.contains(octahedron)
    override def nanobotOverlaps(nanobot: Nanobot, octahedron: Nanobot): Boolean = nanobot.overlaps(octahedron)

    override def getSplits(octahedron: Nanobot): Set[Nanobot] = {
      val Nanobot(pos, radius) = octahedron
      val offset = {
        // rounding corrections by VikeStep
        // Nanobot(Pos3(30, 30, 30), 55) splits should still contains Pos3(12, 12, 12)
        if (radius >= 3)
          (1.0 / 3 * radius).floor.toInt
        else if (radius > 0)
          1
        else
          0
      }
      val newRadius = radius - offset
      val axisOffsets = Set(
        Pos3(-offset, 0, 0),
        Pos3(offset, 0, 0),
        Pos3(0, -offset, 0),
        Pos3(0, offset, 0),
        Pos3(0, 0, -offset),
        Pos3(0, 0, offset),
      )
      val offsets = {
        if (radius == 1)
          // must include when going from radius 1 to radius 0, not to forget about center voxel
          axisOffsets + Pos3(0, 0, 0)
        else
          axisOffsets
      }
      offsets.map(offset => Nanobot(pos + offset, newRadius))
    }

    override def originDistance(octahedron: Nanobot): Int = (octahedron.pos manhattanDistance Pos3(0, 0, 0)) - octahedron.radius
  }

  object BoxSplittingPart2Solution extends NonGreedySplittingPart2Solution {

    private def clamp(min: Int, max: Int)(value: Int): Int = {
      if (value < min)
        min
      else if (value > max)
        max
      else value
    }

    case class Box3(min: Pos3, max: Pos3) {
      def contains(pos: Pos3): Boolean = {
        min.x <= pos.x && pos.x <= max.x &&
          min.y <= pos.y && pos.y <= max.y &&
          min.z <= pos.z && pos.z <= max.z
      }

      def contains(octahedron: Nanobot): Boolean = octahedron.corners.forall(contains)

      def closestTo(pos: Pos3): Pos3 = {
        Pos3(
          clamp(min.x, max.x)(pos.x),
          clamp(min.y, max.y)(pos.y),
          clamp(min.z, max.z)(pos.z),
        )
      }
    }

    implicit class NanobotBox3(nanobot: Nanobot) {
      def contains(box: Box3): Boolean = nanobot.contains(box.min) && nanobot.contains(box.max) // TODO: sufficient to only check two corners?

      def overlaps(box: Box3): Boolean = nanobot.contains(box.closestTo(nanobot.pos))
    }

    override type A = Box3

    override def getInitial(nanobots: Seq[Nanobot]): Box3 = {
      //val initPos = Pos3(0, 0, 0)
      val poss = nanobots.map(_.pos)
      val initX = (poss.map(_.x).min + poss.map(_.x).max) / 2
      val initY = (poss.map(_.y).min + poss.map(_.y).max) / 2
      val initZ = (poss.map(_.z).min + poss.map(_.z).max) / 2
      val initPos = Pos3(initX, initY, initZ)
      Iterator.iterate(1)(_ * 2).map({ radius =>
        Box3(initPos + Pos3(-radius, -radius, -radius), initPos + Pos3(radius, radius, radius))
      }).find(box => nanobots.forall(box.contains)).get
    }


    override def nanobotContains(nanobot: Nanobot, box: Box3): Boolean = nanobot.contains(box)
    override def nanobotOverlaps(nanobot: Nanobot, box: Box3): Boolean = nanobot.overlaps(box)

    implicit class FloorDivideInt(n: Int) {
      def floorDiv(d: Int): Int = Math.floorDiv(n, d)
    }

    override def getSplits(box: Box3): Set[Box3] = {
      val Box3(min, max) = box
      if (min.x == max.x)
        return Set()
      val mid = Pos3((min.x + max.x) floorDiv 2, (min.y + max.y) floorDiv 2, (min.z + max.z) floorDiv 2)
      Set(
        Box3(Pos3(min.x, min.y, min.z), Pos3(mid.x, mid.y, mid.z)),
        Box3(Pos3(mid.x + 1, min.y, min.z), Pos3(max.x, mid.y, mid.z)),
        Box3(Pos3(min.x, mid.y + 1, min.z), Pos3(mid.x, max.y, mid.z)),
        Box3(Pos3(min.x, min.y, mid.z + 1), Pos3(mid.x, mid.y, max.z)),
        Box3(Pos3(mid.x + 1, mid.y + 1, min.z), Pos3(max.x, max.y, mid.z)),
        Box3(Pos3(min.x, mid.y + 1, mid.z + 1), Pos3(mid.x, max.y, max.z)),
        Box3(Pos3(mid.x + 1, min.y, mid.z + 1), Pos3(max.x, mid.y, max.z)),
        Box3(Pos3(mid.x + 1, mid.y + 1, mid.z + 1), Pos3(max.x, max.y, max.z)),
      ) ensuring(_.forall(box => box.min.x <= box.max.x && box.min.y <= box.max.y && box.min.z <= box.max.z), box)
    }

    override def originDistance(box: Box3): Int = box.closestTo(Pos3(0, 0, 0)) manhattanDistance Pos3(0, 0, 0)
  }


  private val nanobotRegex = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

  def parseNanobot(s: String): Nanobot = s match {
    case nanobotRegex(x, y, z, r) => Nanobot(Pos3(x.toInt, y.toInt, z.toInt), r.toInt)
  }

  def parseInput(input: String): Seq[Nanobot] = input.lines.map(parseNanobot).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import OctahedronSplittingPart2Solution._
    println(nanobotsInLargestRadius(parseInput(input)))
    println(closestMostNanobots(parseInput(input)))

    // 90702904 - too high
  }
}
