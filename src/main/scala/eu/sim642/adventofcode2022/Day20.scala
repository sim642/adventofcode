package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.IntegralImplicits.*

import scala.collection.mutable

object Day20 {

  trait Part {

    val rounds: Int

    def mix(file: Vector[Long]): Vector[Long] = {
      val buf = file.zipWithIndex.toBuffer
      for (_ <- 0 until rounds) {
        for (fileI <- file.indices) {
          val i = buf.indexWhere(_._2 == fileI)
          val elem@(value, _) = buf.remove(i)
          val newI = ((i + value) %+ buf.size).toInt // modulus temporarily smaller by 1!
          buf.insert(newI, elem)
        }
      }
      buf.toVector.map(_._1)
    }

    def groveCoordinates(file: Vector[Long]): Long = {
      val i0 = file.indexOf(0)
      (1000 to 3000 by 1000)
        .view
        .map(offset => file((i0 + offset) %+ file.size))
        .sum
    }

    def mixGroveCoordinates(file: Vector[Int]): Long = groveCoordinates(mix(file.map(_.toLong)))
  }

  object Part1 extends Part {
    override val rounds: Int = 1
  }

  object Part2 extends Part {
    val decryptionKey = 811589153

    override val rounds: Int = 10

    override def mix(file: Vector[Long]): Vector[Long] = super.mix(file.map(_ * decryptionKey))
  }


  def parseFile(input: String): Vector[Int] = input.linesIterator.map(_.toInt).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.mixGroveCoordinates(parseFile(input)))
    println(Part2.mixGroveCoordinates(parseFile(input)))

    // part 1: 3460 - too low (assuming distinct)
    // part 2: -14083506572009 - not right
  }
}
