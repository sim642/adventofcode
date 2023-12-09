package eu.sim642.adventofcode2016

object Day16 {

  def generateData(initial: String, length: Int): String = {
    val sb = new StringBuilder(length, initial)
    while (sb.length < length) {
      //val s = sb.mkString
      sb.append('0')
      //sb.append(s.reverseMap({ case '0' => '1' case '1' => '0' }))
      //s.reverseIterator.take(length - sb.length).map({ case '0' => '1' case '1' => '0' }).foreach(sb.append)
      sb.view.slice(0, sb.length - 1).reverseIterator.take(length - sb.length).map({ case '0' => '1' case '1' => '0' }).foreach(sb.append)
    }
    //sb.substring(0, length)
    sb.mkString
  }

  /*def checksum(data: String): String = {

    def reduce(it: Iterator[Char]): Iterator[Char] = {
      it.grouped(2).map(s => if (s(0) == s(1)) '1' else '0')
    }

    def helper(it: Iterator[Char], length: Int): Iterator[Char] = {
      if (length % 2 == 0)
        helper(reduce(it), length / 2)
      else
        it
    }

    helper(data.iterator, data.length).mkString
  }*/

  def checksum(data: String): String = {
    // https://old.reddit.com/r/adventofcode/comments/5imh3d/2016_day_16_solutions/db9erfp/
    val mergeSize = Integer.lowestOneBit(data.length) // largest power of two divisor
    data.iterator
      .grouped(mergeSize)
      .map(s => if (s.count(_ == '1') % 2 == 0) '1' else '0') // checksumming two chars is XNOR
      .mkString
  }

  def fillChecksum(initial: String, length: Int): String = checksum(generateData(initial, length))

  //lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim
  val input = "00101000101111010"

  val part1Length = 272
  val part2Length = 35651584

  def main(args: Array[String]): Unit = {
    println(fillChecksum(input, part1Length))
    println(fillChecksum(input, part2Length)) // TODO: optimize without generating entire string
  }

  /*
  10000 0 11110 0 10000 1 11|110
   0 1  1  1 1  1  0 1  0  1| 1
     0     1    1    0     0
   */
}
