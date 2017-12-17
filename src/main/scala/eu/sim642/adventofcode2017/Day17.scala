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

  lazy val input: Int = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim.toInt

  def main(args: Array[String]): Unit = {
    println(spinlockAfter(input))
  }
}
