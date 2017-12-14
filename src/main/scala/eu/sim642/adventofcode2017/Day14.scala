package eu.sim642.adventofcode2017

object Day14 {

  def hashRow(key: String, row: Int): Seq[Byte] = Day10.knotHash(s"$key-$row").map(_.toByte)

  def bytes2bits(bytes: Seq[Byte]): Seq[Boolean] =
    bytes.flatMap(byte => (0 until 8).foldLeft(List.empty[Boolean])((acc, i) => (((byte >> i) & 1) != 0) :: acc))

  def squaresUsed(key: String): Int = {
    val rows = (0 until 128).map(row => bytes2bits(hashRow(key, row)))
    rows.map(_.count(x => x)).sum
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(squaresUsed(input))
  }
}
