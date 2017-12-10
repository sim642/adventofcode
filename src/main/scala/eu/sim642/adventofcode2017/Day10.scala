package eu.sim642.adventofcode2017

object Day10 {

  case class KnotState(elems: Vector[Int] = Vector.range(0, 256),
                       pos: Int = 0,
                       skipSize: Int = 0) {

    def reverse(length: Int): KnotState = {
      val buf = elems.toBuffer
      for (i <- 0 until length) {
        buf((pos + i) % buf.size) = elems((pos + length - i - 1) % buf.size)
      }

      KnotState(buf.toVector, (pos + length + skipSize) % buf.size, skipSize + 1)
    }

    def hash: Int = elems.take(2).product
  }

  def simulate(knotState: KnotState, lengths: Seq[Int]): KnotState = lengths.foldLeft(knotState)(_.reverse(_))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim
  lazy val inputLengths: Seq[Int] = input.split(",").toSeq.map(_.toInt)

  def main(args: Array[String]): Unit = {
    println(simulate(KnotState(), inputLengths).hash)
  }
}
