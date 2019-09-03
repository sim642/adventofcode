package eu.sim642.adventofcode2015

import ujson.{Arr, Num, Obj, Str}

object Day12 {

  trait Part {
    def sumNumbers(json: ujson.Value): Int

    def sumNumbers(input: String): Int = sumNumbers(ujson.read(input))
  }

  object Part1 extends Part {
    override def sumNumbers(json: ujson.Value): Int = json match {
      case Num(value) => value.toInt
      case Arr(value) => value.map(sumNumbers).sum
      case Obj(value) => value.values.map(sumNumbers).sum
      case _ => 0
    }
  }

  object Part2 extends Part {
    override def sumNumbers(json: ujson.Value): Int = json match {
      case Num(value) => value.toInt
      case Arr(value) => value.map(sumNumbers).sum
      case Obj(value) =>
        val ignore = value.values.exists({
          case Str("red") => true
          case _ => false
        })

        if (ignore)
          0
        else
          value.values.map(sumNumbers).sum
      case _ => 0
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumNumbers(input))
    println(Part2.sumNumbers(input))
  }
}
