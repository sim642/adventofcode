package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.IntegralImplicits._
import eu.sim642.adventofcodelib.SeqImplicits._

object Day20 {

  def mixOne(fileIndex: Vector[(Int, Int)], i0: Int): Vector[(Int, Int)] = {
    /*val i = file.indexOf(value)
    val (init, _ +: tail) = file.splitAt(i): @unchecked
    val newI = (i + value) %+ file.size
    val (init2, tail2) = (init ++ tail).splitAt(newI + (if (value < 0) -1 else 0))
    init2 ++ (value +: tail2)*/
    //val i = file.indexOf(value)
    val i = fileIndex.indexWhere(_._2 == i0)
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
    v5
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


  def parseFile(input: String): Vector[Int] = input.linesIterator.map(_.toInt).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(mixGroveCoordinates(parseFile(input)))

    // part 1: 3460 - too low (assuming distinct)
  }
}
