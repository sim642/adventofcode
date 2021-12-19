package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.pos.{Pos, Pos3}
import eu.sim642.adventofcodelib.IteratorImplicits.*

import scala.annotation.tailrec

object Day19 {

  def posOrientations(pos: Pos3): Seq[Pos3] = {
    val Pos3(x, y, z) = pos
    Seq(
      Pos3(x, y, z),
      Pos3(-y, x, z),
      Pos3(-x, -y, z),
      Pos3(y, -x, z),

      Pos3(-x, y, -z),
      Pos3(y, x, -z),
      Pos3(x, -y, -z),
      Pos3(-y, -x, -z),

      Pos3(-z, y, x),
      Pos3(-z, x, -y),
      Pos3(-z, -y, -x),
      Pos3(-z, -x, y),

      Pos3(z, y, -x),
      Pos3(z, x, y),
      Pos3(z, -y, x),
      Pos3(z, -x, -y),

      Pos3(x, -z, y),
      Pos3(-y, -z, x),
      Pos3(-x, -z, -y),
      Pos3(y, -z, -x),

      Pos3(x, z, -y),
      Pos3(-y, z, -x),
      Pos3(-x, z, y),
      Pos3(y, z, x),
    )
  }

  //private val angles = Seq(Pos(0, 1), Pos(1, 0), Pos(0, -1), Pos(-1, 0)) // sin, cos at 0, 90, 180, 270 degrees

  /* def iterateScannerOrientations(scanner: Set[Pos3]): Iterator[Set[Pos3]] = {
    val mats = (for {
      p@Pos3(x, y, z) <- Seq(
        Pos3(1, 0, 0),
        //Pos3(-1, 0, 0),
        Pos3(0, 1, 0),
        //Pos3(0, -1, 0),
        Pos3(0, 0, 1),
        //Pos3(0, 0, -1),
      ).iterator
      a@Pos(s, c) <- angles.iterator
    } yield {
      val C = 1 - c
      val Q = Seq(
        Seq(x * x * C + c    , x * y * C - z * s, x * z * C + y * s),
        Seq(y * x * C + z * s, y * y * C + c    , y * z * C - x * s),
        Seq(z * x * C - y * s, z * y * C + x * s, z * z * C + c    ),
      )
      println((p, a, Q))
      Q
    }).toSet
    println("---------")
    mats.foreach(println)
    println(mats.size)

    for {
      Pos3(x, y, z) <- Pos3.axisOffsets.iterator
      Pos(s, c) <- angles.iterator
    } yield {
      val C = 1 - c
      val Q = Seq(
        Seq(x * x * C + c    , x * y * C - z * s, x * z * C + y * s),
        Seq(y * x * C + z * s, y * y * C + c    , y * z * C - x * s),
        Seq(z * x * C - y * s, z * y * C + x * s, z * z * C + c    ),
      )
      println(Q)
      val q = { (p: Pos3) =>
        Pos3(
          p.x * Q(0)(0) + p.y * Q(0)(1) + p.z * Q(0)(2),
          p.x * Q(1)(0) + p.y * Q(1)(1) + p.z * Q(1)(2),
          p.x * Q(2)(0) + p.y * Q(2)(1) + p.z * Q(2)(2),
        )
      }
      scanner.map(q)
    }
  } */

  def scannerOrientations(scanner: Set[Pos3]): Seq[Set[Pos3]] = {
    scanner.toSeq.map(posOrientations).transpose.map(_.toSet)
  }

  def matchScanner(scanner1: Set[Pos3], scanner2: Set[Pos3]): Option[(Set[Pos3], Set[Pos3], Pos3)] = {
    (for {
      scanner2 <- scannerOrientations(scanner2).iterator
      p1 <- scanner1.iterator
      p2 <- scanner2.iterator
      d = p1 - p2 // this way fits with examples
      //intersect2 = scanner2 & scanner1.map(_ - d)
      intersect2 = scanner2.map(_ + d) & scanner1 // faster this way because & filters left and looks up right
      //() = println(intersect2.size)
      if intersect2.size >= 12
    } yield (scanner2, intersect2, d)).headOption
  }

  def solve(scanners: Seq[Set[Pos3]]): Set[Pos3] = {

    @tailrec
    def helper(scanners: Seq[(Set[Pos3], Int)], beacons: Set[Pos3]): Set[Pos3] = {
      if (scanners.isEmpty)
        beacons
      else {
        val ((scanner, i), (scanner2, _, d)) = (for {
          (scanner, i) <- scanners.iterator
          m <- matchScanner(beacons, scanner)
        } yield ((scanner, i), m)).head
        println(s"match $i")
        val newBeacons = beacons ++ scanner2.map(_ + d)
        val newScanners = scanners.filterNot(_ == (scanner, i))
        helper(newScanners, newBeacons)
      }
    }

    val (scanner0, _) +: rest = scanners.zipWithIndex
    helper(rest, scanner0)
  }

  def countBeacons(scanners: Seq[Set[Pos3]]): Int = solve(scanners).size


  def parseScanner(s: String): Set[Pos3] = {
    s.linesIterator
      .drop(1)
      .map(line => {
        val Seq(x, y, z) = line.split(",", 3).toSeq
        Pos3(x.toInt, y.toInt, z.toInt)
      })
      .toSet
  }

  def parseScanners(input: String): Seq[Set[Pos3]] = input.split("\n\n").iterator.map(parseScanner).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBeacons(parseScanners(input)))
  }
}
