package eu.sim642.adventofcode2017

object Day15 {

  trait Part {
    def generatorA(start: Int): Iterator[Int]
    def generatorB(start: Int): Iterator[Int]

    protected val defaultPairs: Int

    def matchesCount(startA: Int, startB: Int, pairs: Int = defaultPairs): Int = {
      generatorA(startA).zip(generatorB(startB)).take(pairs).count({ case (a, b) => (a & 0xFFFF) == (b & 0xFFFF) })
    }
  }

  object Part1 extends Part {
    private def generator(start: Int, factor: Int): Iterator[Int] =
      Iterator.iterate(start)(value => ((value.toLong * factor) % 2147483647).toInt).drop(1)

    override def generatorA(start: Int): Iterator[Int] = generator(start, 16807)
    override def generatorB(start: Int): Iterator[Int] = generator(start, 48271)

    override protected val defaultPairs: Int = 40000000
  }

  object Part2 extends Part {
    private def multipleIterator(it: Iterator[Int], multiple: Int): Iterator[Int] = it.filter(_ % multiple == 0)

    override def generatorA(start: Int): Iterator[Int] = multipleIterator(Part1.generatorA(start), 4)
    override def generatorB(start: Int): Iterator[Int] = multipleIterator(Part1.generatorB(start), 8)

    override protected val defaultPairs: Int = 5000000
  }

  private val inputRegex =
    """Generator A starts with (\d+)
      |Generator B starts with (\d+)""".stripMargin.r

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim
  lazy val (inputStartA, inputStartB) = input match {
    case inputRegex(a, b) => (a.toInt, b.toInt)
  }

  def main(args: Array[String]): Unit = {
    println(Part1.matchesCount(inputStartA, inputStartB))
    println(Part2.matchesCount(inputStartA, inputStartB))
  }
}
