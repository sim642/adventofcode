package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.LazyListImplicits._

object Day2 {

  case class Program(code: Vector[Int], i: Int = 0) {
    def execOne: Option[Program] = {
      code(i) match {
        case 1 =>
          val in1 = code(i + 1)
          val in2 = code(i + 2)
          val out = code(i + 3)
          val newCode = code.updated(out, code(in1) + code(in2))
          Some(Program(newCode, i + 4))
        case 2 =>
          val in1 = code(i + 1)
          val in2 = code(i + 2)
          val out = code(i + 3)
          val newCode = code.updated(out, code(in1) * code(in2))
          Some(Program(newCode, i + 4))
        case 99 => None
        case _ => throw new IllegalArgumentException("Unknown opcode")
      }
    }

    def execAll: LazyList[Program] = {
      LazyList.unfold0(this)(_.execOne)
    }

    def execFinal: Program = {
      execAll.last
    }
  }

  def execPosition0(code: Vector[Int]): Int = {
    val newCode = code.updated(1, 12).updated(2, 2)
    Program(newCode).execFinal.code(0)
  }

  def parseCode(input: String): Vector[Int] = input.split(',').toVector.map(_.toInt)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(execPosition0(parseCode(input)))
  }
}
