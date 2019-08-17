package eu.sim642.adventofcode2018

import eu.sim642.adventofcodelib.box.{Box3, Box4}
import eu.sim642.adventofcodelib.pos.Pos3
import eu.sim642.adventofcodelib.pos.Pos4

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
      maximumOverlap(nanobots).map(n => (n.pos manhattanDistance Pos3.zero) - n.radius).max
    }
  }

  object FourDimCliquePart2Solution extends Part2Solution {

    implicit class ExactDivideInt(n: Int) {
      def /!(d: Int): Option[Int] = if (n % d == 0) Some(n / d) else None
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
      pos3s.map(p => p manhattanDistance Pos3.zero).min
    }
  }

  object OctahedronSplittingPart2Solution extends Part2Solution {
    def getInitialOctahedron(nanobots: Seq[Nanobot]): Nanobot = {
      //val initPos = Pos3.zero
      val poss = nanobots.map(_.pos)
      val initX = (poss.map(_.x).min + poss.map(_.x).max) / 2
      val initY = (poss.map(_.y).min + poss.map(_.y).max) / 2
      val initZ = (poss.map(_.z).min + poss.map(_.z).max) / 2
      val initPos = Pos3(initX, initY, initZ)
      val radius = nanobots.map(nanobot => (initPos manhattanDistance nanobot.pos) + nanobot.radius).max
      Nanobot(initPos, radius)
    }

    def getBounds(nanobots: Seq[Nanobot], octahedron: Nanobot): (Int, Int) = {
      val lower = nanobots.count(_.contains(octahedron))
      val upper = nanobots.count(_.overlaps(octahedron))
      (lower, upper)
    }

    def getSplits(octahedron: Nanobot): Set[Nanobot] = {
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
          axisOffsets + Pos3.zero
        else
          axisOffsets
      }
      offsets.map(offset => Nanobot(pos + offset, newRadius))
    }

    def closestMostNanobots(nanobots: Seq[Nanobot]): Int = {
      val queue: mutable.PriorityQueue[(Nanobot, (Int, Int), Int)] =
        mutable.PriorityQueue.empty(Ordering.by({ case (octahedron, (lower, upper), originDist) =>
          (upper, lower, -originDist)
          //(upper, -octahedron.radius, -originDist) // much faster but possibly incorrect?
        }))
      val done: mutable.Set[Nanobot] = mutable.Set.empty

      def enqueue(octahedron: Nanobot): Unit = {
        queue.enqueue((octahedron, getBounds(nanobots, octahedron), (octahedron.pos manhattanDistance Pos3.zero) - octahedron.radius))
      }

      val initialOctahedron = getInitialOctahedron(nanobots)
      enqueue(initialOctahedron)

      breakable {
        while (queue.nonEmpty) {
          val (octahedron, (lower, upper), originDist) = queue.dequeue()
          if (!done.contains(octahedron)) {
            done += octahedron

            if (lower == upper)
              return originDist

            for (splitOctahedron <- getSplits(octahedron))
              enqueue(splitOctahedron)
          }
        }
      }
      ???
    }
  }

  object BoxSplittingPart2Solution extends Part2Solution {

    private def clamp(min: Int, max: Int)(value: Int): Int = {
      if (value < min)
        min
      else if (value > max)
        max
      else value
    }

    implicit class NanobotBox3(box: Box3) {
      val Box3(min, max) = box

      def contains(octahedron: Nanobot): Boolean = octahedron.corners.forall(box.contains)

      def closestTo(pos: Pos3): Pos3 = {
        Pos3(
          clamp(min.x, max.x)(pos.x),
          clamp(min.y, max.y)(pos.y),
          clamp(min.z, max.z)(pos.z),
        )
      }
    }

    implicit class Box3Nanobot(nanobot: Nanobot) {
      def contains(box: Box3): Boolean = nanobot.contains(box.min) && nanobot.contains(box.max) // TODO: sufficient to only check two corners?

      def overlaps(box: Box3): Boolean = nanobot.contains(box.closestTo(nanobot.pos))
    }

    def getInitialBox(nanobots: Seq[Nanobot]): Box3 = {
      //val initPos = Pos3.zero
      val poss = nanobots.map(_.pos)
      val initX = (poss.map(_.x).min + poss.map(_.x).max) / 2
      val initY = (poss.map(_.y).min + poss.map(_.y).max) / 2
      val initZ = (poss.map(_.z).min + poss.map(_.z).max) / 2
      val initPos = Pos3(initX, initY, initZ)
      Iterator.iterate(1)(_ * 2).map({ radius =>
        Box3(initPos + Pos3(-radius, -radius, -radius), initPos + Pos3(radius, radius, radius))
      }).find(box => nanobots.forall(box.contains(_))).get
    }

    def getBounds(nanobots: Seq[Nanobot], box: Box3): (Int, Int) = {
      val lower = nanobots.count(_.contains(box))
      val upper = nanobots.count(_.overlaps(box))
      (lower, upper)
    }

    implicit class FloorDivideInt(n: Int) {
      def floorDiv(d: Int): Int = Math.floorDiv(n, d)
    }

    def getSplits(box: Box3): Set[Box3] = {
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

    def closestMostNanobots(nanobots: Seq[Nanobot]): Int = {
      val queue: mutable.PriorityQueue[(Box3, (Int, Int), Int)] =
        mutable.PriorityQueue.empty(Ordering.by({ case (octahedron, (lower, upper), originDist) =>
          (upper, lower, -originDist)
          //(upper, -octahedron.radius, -originDist) // much faster but possibly incorrect?
        }))
      val done: mutable.Set[Box3] = mutable.Set.empty

      def enqueue(box: Box3): Unit = {
        queue.enqueue((box, getBounds(nanobots, box), box.closestTo(Pos3.zero) manhattanDistance Pos3.zero))
      }

      val initialBox = getInitialBox(nanobots)
      enqueue(initialBox)

      var closestBox: Option[(Box3, Int, Int)] = None
      breakable {
        while (queue.nonEmpty) {
          val (box, (lower, upper), originDist) = queue.dequeue()
          if (!done.contains(box)) {
            done += box

            if (closestBox.exists(_._2 > upper))
              break()
            else if (lower == upper) {
              closestBox match {
                case None =>
                  closestBox = Some((box, upper, originDist))
                  //break()
                case Some((_, closestUpper, closestOriginDist)) =>
                  if (originDist < closestOriginDist)
                    closestBox = Some((box, upper, originDist))
              }
            }

            for (splitBox <- getSplits(box))
              enqueue(splitBox)
          }
          //else
          //  println(s"dup: $box")
        }
      }
      closestBox.get._3
    }
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
