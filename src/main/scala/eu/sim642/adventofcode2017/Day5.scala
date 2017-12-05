package eu.sim642.adventofcode2017

import scala.collection.AbstractIterator

object Day5 {

  trait Part {
    def updatedOffset(offset: Int): Int

    case class OffsetState(offsets: Seq[Int], index: Int = 0) {
      def jump: OffsetState = {
        val offset = offsets(index)
        OffsetState(offsets.updated(index, updatedOffset(offset)), index + offset)
      }

      def exited: Boolean = !offsets.indices.contains(index)
    }

    class OffsetIterator(private var offsetState: OffsetState) extends AbstractIterator[OffsetState] {
      override def hasNext: Boolean = {
        !offsetState.exited
      }

      override def next(): OffsetState = {
        val returnState = offsetState
        offsetState = offsetState.jump
        returnState
      }
    }

    def exitSteps(offsets: Seq[Int]): Int = {
      val initialState = OffsetState(offsets)
      val it = new OffsetIterator(initialState)
      it.size
    }

    def exitSteps(offsetsStr: String): Int = {
      val offsets = offsetsStr.lines.map(_.toInt).toIndexedSeq // IndexedSeq is much faster because we index a lot
      exitSteps(offsets)
    }
  }

  object Part1 extends Part {
    override def updatedOffset(offset: Int): Int = offset + 1
  }

  object Part2 extends Part {
    override def updatedOffset(offset: Int): Int = if (offset >= 3) offset - 1 else offset + 1
  }

  val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.exitSteps(input))
    println(Part2.exitSteps(input))
  }
}
