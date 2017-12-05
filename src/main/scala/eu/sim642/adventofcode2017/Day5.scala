package eu.sim642.adventofcode2017

import scala.collection.AbstractIterator

object Day5 {

  case class OffsetState(offsets: Seq[Int], index: Int = 0) {
    def jump: OffsetState = {
      val offset = offsets(index)
      OffsetState(offsets.updated(index, offset + 1), index + offset)
    }

    def exited: Boolean = !offsets.indices.contains(index)
  }

  class OffsetIterator(private var offsetState: OffsetState) extends AbstractIterator[OffsetState] {
    override def hasNext: Boolean = {
      !offsetState.exited
    }

    override def next(): OffsetState = {
      println(offsetState.index)
      val currentState = offsetState
      offsetState = offsetState.jump
      currentState
    }
  }

  def exitSteps(offsets: Seq[Int]): Int = {
    val initialState = OffsetState(offsets)
    val it = new OffsetIterator(initialState)
    it.size
  }

  def exitSteps(offsetsStr: String): Int = {
    val offsets = offsetsStr.lines.map(_.toInt).toSeq
    exitSteps(offsets)
  }

  val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(exitSteps(input))
  }
}
