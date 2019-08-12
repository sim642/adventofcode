package eu.sim642.adventofcode2016

import scala.collection.mutable
import Day14.md5

object Day5 {

  def getPassword(doorId: String, times: Int = 8): String = {
    Iterator.from(0).map(i => md5(doorId + i)).filter(_.startsWith("00000")).map(_(5)).take(times).mkString("")
  }

  def getPassword2(doorId: String): String = {
    val it = Iterator.from(0).map(i => md5(doorId + i)).filter(_.startsWith("00000")).map(s => (s(5), s(6))).filter(p => ('0' to '7').contains(p._1)).map(p => (p._1 - '0', p._2))

    // TODO: nasty imperative solution
    val password = mutable.ArrayBuffer.fill[Option[Char]](8)(None)
    var filled = 0
    for {
      (pos, c) <- it
      if password(pos).isEmpty
    } {
      //println((pos, c))
      //System.out.flush()
      password(pos) = Some(c)
      filled += 1

      if (filled == 8)
        return password.map(_.get).mkString("")
    }

    ???
  }

  val input: String = "abbhdwsy"

  def main(args: Array[String]): Unit = {
    println(getPassword(input))
    println(getPassword2(input))
  }
}
