package eu.sim642.adventofcode2017

import scala.collection.AbstractIterator

object Day15 {

  class Generator(start: Int, factor: Int) extends AbstractIterator[Int] {
    private var value = start

    override def hasNext: Boolean = true

    override def next(): Int = {
      value = ((value.toLong * factor) % 2147483647).toInt
      value
    }
  }

  class GeneratorA(start: Int) extends Generator(start, 16807)
  class GeneratorB(start: Int) extends Generator(start, 48271)

  def matchesCount(startA: Int, startB: Int, pairs: Int = 40000000): Int = {
    new GeneratorA(startA).zip(new GeneratorB(startB)).take(pairs).count({ case (a, b) => (a & 0xFFFF) == (b & 0xFFFF) })
  }

  class DelegateIterator[A](it: Iterator[A]) extends AbstractIterator[A] {
    override def hasNext: Boolean = it.hasNext
    override def next(): A = it.next()
  }

  class MultipleIterator(it: Iterator[Int], multiple: Int) extends DelegateIterator(it.filter(_ % multiple == 0))

  class MultipleGeneratorA(start: Int) extends MultipleIterator(new GeneratorA(start), 4)
  class MultipleGeneratorB(start: Int) extends MultipleIterator(new GeneratorB(start), 8)

  def multipleMatchesCount(startA: Int, startB: Int, pairs: Int = 5000000): Int = {
    new MultipleGeneratorA(startA).zip(new MultipleGeneratorB(startB)).take(pairs).count({ case (a, b) => (a & 0xFFFF) == (b & 0xFFFF) })
  }

  /*private val inputRegex =
    """Generator A starts with (\d+)\n
      |Generator B starts with (\d+)""".r

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim
  lazy val (inputStartA, inputStartB) = input match {
    case inputRegex(a, b) => (a.toInt, b.toInt)
  }*/
  val (inputStartA, inputStartB) = (116, 299)

  def main(args: Array[String]): Unit = {
    println(matchesCount(inputStartA, inputStartB))
    println(multipleMatchesCount(inputStartA, inputStartB))
  }
}
