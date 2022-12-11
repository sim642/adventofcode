package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.NumberTheory
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

import scala.collection.immutable.Queue
import scala.math.Integral.Implicits._

object Day11 {

  type Item = Long

  case class Monkey(operation: Item => Item,
                    testDivisible: Int,
                    testTrue: Int,
                    testFalse: Int,
                    items: Queue[Item])

  trait Part {
    protected val rounds: Long
    protected def makePostOperation(monkeys: Vector[Monkey]): Item => Item

    def runTurn(monkeys: Vector[Monkey], current: Int, postOperation: Item => Item): Vector[Monkey] = {
      val currentMonkey = monkeys(current)

      currentMonkey.items.foldLeft(monkeys)({ (monkeys, item) =>
        val test = postOperation(currentMonkey.operation(item))
        val throwIndex = if (test % currentMonkey.testDivisible == 0) currentMonkey.testTrue else currentMonkey.testFalse
        val throwMonkey = monkeys(throwIndex)
        monkeys.updated(throwIndex, throwMonkey.copy(items = throwMonkey.items.appended(test)))
      }).updated(current, currentMonkey.copy(items = Queue.empty))
    }

    protected def runRounds(initialMonkeys: Vector[Monkey], operation: Item => Item, rounds: Int): (Vector[Monkey], Vector[Int]) = {
      (1 to rounds).foldLeft((initialMonkeys, Vector.fill(initialMonkeys.size)(0)))({ case ((monkeys, inspectCounts), round) =>
        monkeys.indices.foldLeft((monkeys, inspectCounts))({ case ((monkeys, inspectCounts), current) =>
          (runTurn(monkeys, current, operation), inspectCounts.updated(current, inspectCounts(current) + monkeys(current).items.size))
        })
      })
    }

    def monkeyBusiness(initialMonkeys: Vector[Monkey]): Long = {
      val postOperation = makePostOperation(initialMonkeys)
      val (_, finalInspectCounts) = runRounds(initialMonkeys, postOperation, rounds.toInt)
      finalInspectCounts.sorted.takeRight(2).map(_.toLong).product
    }
  }

  object Part1 extends Part {
    override protected val rounds: Long = 20

    override protected def makePostOperation(monkeys: Vector[Monkey]): Item => Item =
      x => x / 3
  }

  trait Part2 extends Part {
    override protected val rounds: Long = 10000

    override protected def makePostOperation(monkeys: Vector[Monkey]): Item => Item = {
      val lcm = NumberTheory.lcm(monkeys.map(_.testDivisible))
      x => x % lcm
    }
  }

  object Part2 extends Part2

  /**
   * https://www.reddit.com/r/adventofcode/comments/zinjei/2022_day_11_unofficial_part_3_nanomonkeys/
   */
  object Part3 extends Part2 {
    override protected val rounds: Long = 86400000000000L

    override def monkeyBusiness(initialMonkeys: Vector[Monkey]): Long = {
      val postOperation = makePostOperation(initialMonkeys)

      val cycle = NaiveCycleFinder.find(initialMonkeys, monkeys =>
        monkeys.indices.foldLeft(monkeys)({ case (monkeys, current) =>
          runTurn(monkeys, current, postOperation)
        })
      )

      val (stemMonkeys, stemInspectCounts) = runRounds(initialMonkeys, postOperation, cycle.stemLength)
      val (_, cycleInspectCounts) = runRounds(stemMonkeys, postOperation, cycle.cycleLength)
      val (cycleRepeat, tailLength) = (rounds - cycle.stemLength) /% cycle.cycleLength
      val (_, tailInspectCounts) = runRounds(stemMonkeys, postOperation, tailLength.toInt)

      val finalInspectCounts = stemInspectCounts.lazyZip(cycleInspectCounts).lazyZip(tailInspectCounts).map({ case (stem, cycle, tail) =>
        stem + cycleRepeat * cycle + tail
      })
      finalInspectCounts.sorted.takeRight(2).sum
    }
  }

  private val monkeyRegex =
    """Monkey \d+:
      |  Starting items: (\d+(?:, \d+)*)
      |  Operation: new = (\d+|old) ([+*]) (\d+|old)
      |  Test: divisible by (\d+)
      |    If true: throw to monkey (\d+)
      |    If false: throw to monkey (\d+)""".stripMargin.r

  def parseMonkey(s: String): Monkey = s match {
    case monkeyRegex(itemsStr, opLeft, op, opRight, testDivisble, testTrue, testFalse) =>
      val items = itemsStr.split(", ").map(_.toLong).to(Queue)

      def operation(old: Item): Item = {
        val left = if (opLeft == "old") old else opLeft.toLong
        val right = if (opRight == "old") old else opRight.toLong
        op match {
          case "+" => left + right
          case "*" => left * right
        }
      }

      Monkey(
        operation = operation,
        testDivisible = testDivisble.toInt,
        testTrue = testTrue.toInt,
        testFalse = testFalse.toInt,
        items = items
      )
  }

  def parseMonkeys(input: String): Vector[Monkey] = input.split("\n\n").map(parseMonkey).toVector


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.monkeyBusiness(parseMonkeys(input)))
    println(Part2.monkeyBusiness(parseMonkeys(input)))

    // part 1: 70176 - too high (Int overflow)
  }
}
