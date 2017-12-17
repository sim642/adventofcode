package eu.sim642.adventofcode2017

object Day17 {

  trait Part {
    trait SpinStateLike[Repr] { this: Repr =>
      def next(step: Int): Repr
      def after: Int
    }
    type SpinState <: SpinStateLike[SpinState]

    protected val initialSpinState: SpinState
    def spinlock(step: Int): Iterator[SpinState] = Iterator.iterate(initialSpinState)(_.next(step))

    protected val defaultSteps: Int
    def spinlockAfter(step: Int, steps: Int = defaultSteps): Int = spinlock(step).drop(steps).next().after
  }

  object Part1 extends Part {
    case class SpinState(buffer: Vector[Int] = Vector(0), pos: Int = 0) extends SpinStateLike[SpinState] {
      override def next(step: Int): SpinState = {
        val insertValue = buffer(pos) + 1
        val insertPos = (pos + step) % buffer.size + 1
        val (prefix, suffix) = buffer.splitAt(insertPos)
        SpinState(prefix ++ (insertValue +: suffix), insertPos)
      }

      override def after: Int = buffer((pos + 1) % buffer.size)
    }

    override protected val initialSpinState: SpinState = SpinState()
    override protected val defaultSteps: Int = 2017
  }

  object Part2 extends Part {
    case class SpinState(size: Int = 1, value1: Option[Int] = None, pos: Int = 0) extends SpinStateLike[SpinState] {
      override def next(step: Int): SpinState = {
        val insertValue = size
        val insertPos = (pos + step) % size + 1
        SpinState(size + 1, if (insertPos == 1) Some(insertValue) else value1, insertPos)
      }

      override def after: Int = value1.get
    }

    override protected val initialSpinState: SpinState = SpinState()
    override protected val defaultSteps: Int = 50000000
  }

  lazy val input: Int = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim.toInt

  def main(args: Array[String]): Unit = {
    println(Part1.spinlockAfter(input))
    println(Part2.spinlockAfter(input))
  }
}
