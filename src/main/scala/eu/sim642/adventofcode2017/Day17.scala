package eu.sim642.adventofcode2017

object Day17 {

  case class SpinState(buffer: Vector[Int] = Vector(0), pos: Int = 0) {
    def next(step: Int): SpinState = {
      val insertValue = buffer(pos) + 1
      val insertPos = (pos + step) % buffer.size + 1
      val (prefix, suffix) = buffer.splitAt(insertPos)
      SpinState(prefix ++ (insertValue +: suffix), insertPos)
    }

    def afterPos: Int = buffer((pos + 1) % buffer.size)
  }

  def spinlock(step: Int): Iterator[SpinState] = Iterator.iterate(SpinState())(_.next(step))

  def spinlockAfter(step: Int, steps: Int = 2017): Int = spinlock(step).drop(steps).next().afterPos

  case class SpinStateZero(size: Int = 1, value1: Option[Int] = None, pos: Int = 0) {
    def next(step: Int): SpinStateZero = {
      val insertValue = size
      val insertPos = (pos + step) % size + 1
      SpinStateZero(size + 1, if (insertPos == 1) Some(insertValue) else value1, insertPos)
    }
  }

  def spinlockZero(step: Int): Iterator[SpinStateZero] = Iterator.iterate(SpinStateZero())(_.next(step))

  def spinlockAfterZero(step: Int, steps: Int = 50000000): Int = spinlockZero(step).drop(steps).next().value1.get

  lazy val input: Int = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim.toInt

  def main(args: Array[String]): Unit = {
    println(spinlockAfter(input))
    println(spinlockAfterZero(input))
  }
}
