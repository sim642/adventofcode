package eu.sim642.adventofcode2016

object Day7 {

  def alternate[A](l: List[A]): (List[A], List[A]) = l match {
    case Nil => (Nil, Nil)
    case x :: xs =>
      val (as, bs) = alternate(xs)
      (x :: bs, as)
  }

  private val abbaRegex = """(.)((?!\1).)\2\1""".r

  def supportsTLS(address: String): Boolean = {
    val (out, in) = alternate(address.split("[\\[\\]]").toList)
    out.exists(abbaRegex.findFirstIn(_).isDefined) &&
      !in.exists(abbaRegex.findFirstIn(_).isDefined)
  }

  def countSupportsTLS(addresses: Seq[String]): Int = addresses.count(supportsTLS)

  def countSupportsTLS(input: String): Int = countSupportsTLS(input.lines.toSeq)


  // https://stackoverflow.com/a/41243911
  private val abaRegex = """(?=(.)((?!\1).)\1)""".r

  def supportsSSL(address: String): Boolean = {
    val (out, in) = alternate(address.split("[\\[\\]]").toList)
    out.exists(abaRegex.findAllMatchIn(_).exists({ m =>
      val bab = m.group(2) + m.group(1) + m.group(2)
      in.exists(_.contains(bab))
    }))
  }

  def countSupportsSSL(addresses: Seq[String]): Int = addresses.count(supportsSSL)

  def countSupportsSSL(input: String): Int = countSupportsSSL(input.lines.toSeq)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countSupportsTLS(input))
    println(countSupportsSSL(input))
  }
}
