package eu.sim642.adventofcode2016

object Day3 {

  def isValidTriangle(a: Int, b: Int, c: Int): Boolean = {
    a + b > c && a + c > b && b + c > a
  }

  trait Part {
    def countPossibleTriangles(triangles: Seq[(Int, Int, Int)]): Int

    def countPossibleTriangles(input: String): Int = countPossibleTriangles(parseInput(input))
  }

  object Part1 extends Part {
    override def countPossibleTriangles(triangles: Seq[(Int, Int, Int)]): Int = triangles.count({ case (a, b, c) => isValidTriangle(a, b, c)})
  }

  object Part2 extends Part {
    def transposeTriangles(triangles: Seq[(Int, Int, Int)]): Seq[(Int, Int, Int)] = {
      triangles.grouped(3).flatMap({ case Seq((a1, b1, c1), (a2, b2, c2), (a3, b3, c3)) =>
        Seq((a1, a2, a3), (b1, b2, b3), (c1, c2, c3))
      }).toSeq
    }

    override def countPossibleTriangles(triangles: Seq[(Int, Int, Int)]): Int = Part1.countPossibleTriangles(transposeTriangles(triangles))
  }

  def parseInput(input: String): Seq[(Int, Int, Int)] = input.linesIterator.map({ line =>
    val parts = line.trim.split(" +").map(_.toInt)
    (parts(0), parts(1), parts(2))
  }).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countPossibleTriangles(input))
    println(Part2.countPossibleTriangles(input))
  }
}
