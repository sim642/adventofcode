package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.pos.{Pos, Pos3}
import eu.sim642.adventofcodelib.IteratorImplicits.*

import scala.annotation.tailrec

object Day19 {

  type Scanner = Set[Pos3]

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

  def scannerOrientations(scanner: Scanner): Seq[Scanner] = {
    scanner.toSeq.map(posOrientations).transpose.map(_.toSet)
  }

  def matchScanner(scanner1: Scanner, scanner2: Scanner): Option[(Scanner, Pos3)] = {
    (for {
      scanner2 <- scannerOrientations(scanner2).iterator
      p1 <- scanner1.iterator
      p2 <- scanner2.iterator
      d = p1 - p2 // this way fits with examples
      if scanner2.view.map(_ + d).filter(scanner1).sizeIs >= 12 // iterate over smaller scanner2, avoid any intermediate collections
    } yield (scanner2, d)).headOption
    // TODO: is this guaranteed to be correct?
    /*(for {
      scanner2 <- scannerOrientations(scanner2).iterator
      ds = (for {
        p1 <- scanner1.iterator
        p2 <- scanner2.iterator
        d = p1 - p2 // this way fits with examples
      } yield d).groupCount(identity)
      (d, cnt) <- ds.iterator
      if cnt >= 12
    } yield (scanner2, d)).headOption*/
  }

  def solve(scanners: Seq[Scanner]): (Scanner, Set[Pos3]) = {

    @tailrec
    def helper(scanners: Seq[Scanner], beacons: Scanner, poss: Set[Pos3]): (Scanner, Set[Pos3]) = {
      if (scanners.isEmpty)
        (beacons, poss)
      else {
        val (scanner, (scanner2, d)) = (for {
          scanner <- scanners.iterator
          m <- matchScanner(beacons, scanner)
        } yield (scanner, m)).head
        val newBeacons = beacons ++ scanner2.map(_ + d)
        val newScanners = scanners.filterNot(_ == scanner)
        val newPoss = poss + d
        helper(newScanners, newBeacons, newPoss)
      }
    }

    val scanner0 +: rest = scanners
    helper(rest, scanner0, Set(Pos3.zero))
  }

  def countBeacons(scanners: Seq[Scanner]): Int = solve(scanners)._1.size

  def largestScannerDistance(scanners: Seq[Scanner]): Int = {
    val (_, poss) = solve(scanners)
    (for {
      p1 <- poss.iterator
      p2 <- poss.iterator
    } yield p1 manhattanDistance p2).max
  }


  def parseScanner(s: String): Scanner = {
    s.linesIterator
      .drop(1)
      .map(line => {
        val Seq(x, y, z) = line.split(",", 3).toSeq
        Pos3(x.toInt, y.toInt, z.toInt)
      })
      .toSet
  }

  def parseScanners(input: String): Seq[Scanner] = input.split("\n\n").iterator.map(parseScanner).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBeacons(parseScanners(input)))
    println(largestScannerDistance(parseScanners(input)))
  }
}
