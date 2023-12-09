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

  def matchScanner(beacons: Scanner, scanner: Scanner): Option[(Scanner, Pos3)] = {
    /*(for {
      orientedScanner <- scannerOrientations(scanner).iterator
      p1 <- beacons.iterator
      p2 <- orientedScanner.iterator
      d = p1 - p2 // this way fits with examples
      if orientedScanner.view.map(_ + d).filter(beacons).sizeIs >= 12 // iterate over smaller scanner2, avoid any intermediate collections
    } yield (orientedScanner.map(_ + d), d)).headOption*/
    // TODO: is this guaranteed to be correct? could differences corresponding to different translations combine to >= 12 without neither being?
    (for {
      orientedScanner <- scannerOrientations(scanner).iterator
      ds = (for {
        p1 <- beacons.iterator
        p2 <- orientedScanner.iterator
        d = p1 - p2 // this way fits with examples
      } yield d).groupCount(identity)
      (d, cnt) <- ds.iterator
      if cnt >= 12
    } yield (orientedScanner.map(_ + d), d)).headOption
  }

  def solve(scanners: Seq[Scanner]): (Scanner, Set[Pos3]) = {

    // on each iteration, add scanners that match the frontier (new beacons discovered by previous iteration)
    @tailrec
    def helper(scanners: Seq[Scanner], beacons: Scanner, frontier: Scanner, scannerPoss: Set[Pos3]): (Scanner, Set[Pos3]) = {
      val newBeacons = beacons ++ frontier
      if (scanners.isEmpty)
        (newBeacons, scannerPoss)
      else {
        val (matchedScanners, orientedScanners, matchedScannerPoss) = (for {
          scanner <- scanners
          (orientedScanner, scannerPos) <- matchScanner(frontier, scanner)
        } yield (scanner, orientedScanner, scannerPos)).unzip3
        val newScanners = scanners.filterNot(matchedScanners.contains)
        val newFrontier = orientedScanners.reduce(_ ++ _)
        val newScannerPoss = scannerPoss ++ matchedScannerPoss
        helper(newScanners, newBeacons, newFrontier, newScannerPoss)
      }
    }

    val scanner0 +: scannersTail = scanners: @unchecked
    helper(scannersTail, Set.empty, scanner0, Set(Pos3.zero)) // start with scanner 0 fixed
  }

  def countBeacons(scanners: Seq[Scanner]): Int = solve(scanners)._1.size

  def largestScannerDistance(scanners: Seq[Scanner]): Int = {
    val (_, scannerPoss) = solve(scanners)
    (for {
      p1 <- scannerPoss.iterator
      p2 <- scannerPoss.iterator
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

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBeacons(parseScanners(input)))
    println(largestScannerDistance(parseScanners(input)))
  }
}
