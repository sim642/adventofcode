package eu.sim642.adventofcode2015

object Day24 {

  def iterateSubseqSum(weights: List[Int], remaining: Int): Iterator[List[Int]] = {
    if (remaining == 0)
      Iterator(Nil)
    else if (remaining < 0)
      Iterator.empty
    else {
      weights match {
        case Nil => Iterator.empty
        case hd :: tl =>
          iterateSubseqSum(tl, remaining) ++
            iterateSubseqSum(tl, remaining - hd).map(hd :: _)
      }
    }
  }

  def idealFirstQE(weights: List[Int], groups: Int): Long = {
    val groupWeight = weights.sum / groups
    iterateSubseqSum(weights, groupWeight)
      .foldLeft((Option.empty[Int], Set.empty[List[Int]]))({ case (acc@(minLength, minSet), seq) =>
        minLength match {
          case None => (Some(seq.length), Set(seq))
          case Some(minLength) =>
            val seqLength = seq.length
            if (seqLength < minLength)
              (Some(seqLength), Set(seq))
            else if (seqLength == minLength)
              (Some(seqLength), minSet + seq)
            else
              acc
        }
      })
      ._2.map(_.map(_.toLong).product).min
  }

  def idealFirstQE(input: String, groups: Int): Long = idealFirstQE(parseWeights(input), groups)


  def parseWeights(input: String): List[Int] = input.linesIterator.map(_.toInt).toList

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(idealFirstQE(input, 3))
    println(idealFirstQE(input, 4))
  }
}
