package eu.sim642.adventofcode2021

object Day3 {

  type Binary = Seq[Boolean]

  def binary2int(binary: Binary): Int = {
    binary.foldLeft(0)({ case (acc, bit) => (acc << 1) | (if bit then 1 else 0) })
  }

  def gammaRate(binaries: Seq[Binary]): Binary = {
    binaries.transpose.map(_.count(_ == true) >= binaries.size / 2)
  }

  def powerConsumption(binaries: Seq[Binary]): Int = {
    val gamma = gammaRate(binaries)
    val epsilon = gamma.map(!_)
    binary2int(gamma) * binary2int(epsilon)
  }


  def parseBinary(s: String): Binary = s.map(_ == '1')

  def parseBinaries(input: String): Seq[Binary] = input.linesIterator.map(parseBinary).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(powerConsumption(parseBinaries(input)))
  }
}
