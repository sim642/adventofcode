package eu.sim642.adventofcode2022

import eu.sim642.adventofcode2018.Day9.CircularZipper
import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.IntegralImplicits.*
import eu.sim642.adventofcodelib.SeqImplicits.*

import scala.collection.mutable

object Day20 {

  object Part1 {

    def mixOne(fileIndex: Vector[(Int, Int)], i0: Int): Vector[(Int, Int)] = {
      val i = fileIndex.indexWhere(_._2 == i0)
      val value = fileIndex(i)._1
      val (init, _ +: tail) = fileIndex.splitAt(i): @unchecked
      val newI = (i + value) %+ (fileIndex.size - 1)
      val (init2, tail2) = (init ++ tail).splitAt(newI)
      val r = init2 ++ ((value, i0) +: tail2)
      //println((init, tail))
      //println(newI)
      //println((init2, tail2))
      //println(r)
      r
      //val i = file.indexOf(value)
      /*val i = fileIndex.indexWhere(_._2 == i0)
      val value = fileIndex(i)._1
      val newI = (i + value + (if (value < 0) -1 else 0)) %+ fileIndex.size
      //println(s"$i $newI $value")
      //(value +: file.rotateLeft(i).tail.rotateLeft(value)).rotateRight(newI + (if (value < 0) -1 else 0))
      val v1 = fileIndex.rotateLeft(i)
      val v2 = v1.tail
      val v3 = v2.rotateLeft(value)
      val v4 = (value, i0) +: v3
      val v5 = v4.rotateRight(newI + (if (i + value >= fileIndex.size) 1 else 0))
      //println(v1)
      //println(v2)
      //println(v3)
      //println(v4)
      //println(v5)
      v5*/
    }

    def iterateMix(file: Vector[Int]): Iterator[Vector[Int]] = {
      file.indices.iterator.scanLeft(file.zipWithIndex)(mixOne).map(_.map(_._1))
    }

    def mix(file: Vector[Int]): Vector[Int] = iterateMix(file).last

    def groveCoordinates(file: Vector[Int]): Int = {
      val i = file.indexOf(0)
      (1000 to 3000 by 1000).view.map(o => (i + o) %+ file.size).map(file).sum
    }

    def mixGroveCoordinates(file: Vector[Int]): Int = groveCoordinates(mix(file))
  }

  object Part2 {

    def mixOne(fileIndex: Vector[(Long, Int)], i0: Int): Vector[(Long, Int)] = {
      val i = fileIndex.indexWhere(_._2 == i0)
      val value = fileIndex(i)._1
      val (init, _ +: tail) = fileIndex.splitAt(i): @unchecked
      val newI = (i.toLong + value) %+ (fileIndex.size.toLong - 1)
      val (init2, tail2) = (init ++ tail).splitAt(newI.toInt)
      val r = init2 ++ ((value, i0) +: tail2)
      //println((init, tail))
      //println(newI)
      //println((init2, tail2))
      //println(r)
      r
      /*val i = file.indexOf(value)
    val (init, _ +: tail) = file.splitAt(i): @unchecked
    val newI = (i + value) %+ file.size
    val (init2, tail2) = (init ++ tail).splitAt(newI + (if (value < 0) -1 else 0))
    init2 ++ (value +: tail2)*/
      //val i = file.indexOf(value)
      /*val i = fileIndex.indexWhere(_._2 == i0)
      val value = fileIndex(i)._1
      val newI = (i + value + (if (value < 0) -1 else 0)) %+ fileIndex.size
      //println(s"$i $newI $value")
      //(value +: file.rotateLeft(i).tail.rotateLeft(value)).rotateRight(newI + (if (value < 0) -1 else 0))
      val v1 = fileIndex.rotateLeft(i)
      val v2 = v1.tail
      val v3 = v2.rotateLeft((value %+ v2.size).toInt)
      val v4 = (value, i0) +: v3
      val v5 = v4.rotateRight(newI.toInt + (if (i + value >= fileIndex.size) (i + value) / fileIndex.size else 0).toInt)
      //println(v1)
      //println(v2)
      //println(v3)
      //println(v4)
      //println(v5)
      v5*/
    }

    def iterateMix(file: Vector[(Long, Int)]): Iterator[Vector[(Long, Int)]] = {
      file.indices.iterator.scanLeft(file)(mixOne)
    }

    def mix(file: Vector[(Long, Int)]): Vector[(Long, Int)] = {
      //println(s"before: $file")
      val r = iterateMix(file).last
      //println(s"after: $r")
      r
    }

    def iterateMixRounds(file: Vector[Int]): Iterator[Vector[(Long, Int)]] = {
      val init = file.map(_ * 811589153L).zipWithIndex
      Iterator.iterate(init)(mix)
    }

    def mixRounds(file: Vector[Int], rounds: Int = 10): Vector[Long] = iterateMixRounds(file)(rounds).map(_._1)

    def groveCoordinates(file: Vector[Long]): Long = {
      val i = file.indexOf(0)
      (1000 to 3000 by 1000).view.map(o => (i + o) %+ file.size).map(file).sum
    }

    def mixGroveCoordinates(file: Vector[Int]): Long = groveCoordinates(mixRounds(file))
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
