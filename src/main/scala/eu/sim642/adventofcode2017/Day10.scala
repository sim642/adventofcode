package eu.sim642.adventofcode2017

object Day10 {

  case class KnotState(elems: Vector[Int] = Vector.range(0, 256),
                       pos: Int = 0,
                       skipSize: Int = 0) {

    def reversed(length: Int): KnotState = {
      val buf = elems.toBuffer
      for (i <- 0 until length) {
        buf((pos + i) % buf.size) = elems((pos + length - i - 1) % buf.size)
      }

      KnotState(buf.toVector, (pos + length + skipSize) % buf.size, skipSize + 1)
    }

    def checkProduct: Int = elems.take(2).product
  }

  def simulate(initialKnotState: KnotState, lengths: Seq[Int]): KnotState = lengths.foldLeft(initialKnotState)(_.reversed(_))

  def knotCheckProduct(input: String): Int = {
    val lengths = input.split(",").toSeq.map(_.toInt)
    simulate(KnotState(), lengths).checkProduct
  }

  def asciiLengths(input: String): Seq[Int] = input.map(_.toInt) ++ Seq(17, 31, 73, 47, 23)

  def simulate64(initialKnotState: KnotState, lengths: Seq[Int]): KnotState = {
    (0 until 64).foldLeft(initialKnotState) { (knotState, round) =>
      simulate(knotState, lengths)
    }
  }

  def sparse2dense(sparse: Seq[Int]): Seq[Int] = sparse.grouped(16).map(_.reduce(_ ^ _)).toSeq

  def knotHash(input: String): Seq[Int] = {
    sparse2dense(simulate64(KnotState(), asciiLengths(input)).elems)
  }

  def mkHexString(dense: Seq[Int]): String = dense.map(_.formatted("%02x")).mkString

  def knotHashHex(input: String): String = mkHexString(knotHash(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(knotCheckProduct(input))
    println(knotHashHex(input))
  }
}
