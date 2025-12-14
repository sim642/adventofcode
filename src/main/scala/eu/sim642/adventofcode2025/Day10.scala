package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.IteratorImplicits.*
import com.microsoft.z3.{ArithExpr, Context, IntExpr, IntSort, Status}
import eu.sim642.adventofcodelib.NumberTheory
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, TargetNode, UnitNeighbors}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object Day10 {

  type Lights = Vector[Boolean]
  type Buttons = Seq[Seq[Int]]
  type Joltages = Vector[Int]

  case class Machine(lights: Lights, buttons: Buttons, joltages: Joltages)

  trait Part {
    def fewestPresses(machine: Machine): Int

    def sumFewestPresses(machines: Seq[Machine]): Int = machines.map(fewestPresses).sum
  }

  object Part1 extends Part {
    override def fewestPresses(machine: Machine): Int = {
      val graphSearch = new GraphSearch[Lights] with UnitNeighbors[Lights] with TargetNode[Lights] {
        override val startNode: Lights = machine.lights.map(_ => false)

        override def unitNeighbors(lights: Lights): IterableOnce[Lights] =
          machine.buttons.map(_.foldLeft(lights)((acc, i) => acc.updated(i, !acc(i))))

        override val targetNode: Lights = machine.lights
      }

      BFS.search(graphSearch).target.get._2
    }
  }

  trait Part2Solution extends Part

  /**
   * Solution, which naively finds fewest presses by BFS.
   * Does not scale to inputs.
   */
  object NaivePart2Solution extends Part2Solution {
    override def fewestPresses(machine: Machine): Int = {
      val graphSearch = new GraphSearch[Joltages] with UnitNeighbors[Joltages] with TargetNode[Joltages] {
        override val startNode: Joltages = machine.joltages.map(_ => 0)

        override def unitNeighbors(joltages: Joltages): IterableOnce[Joltages] =
          machine.buttons.map(_.foldLeft(joltages)((acc, i) => acc.updated(i, acc(i) + 1)))

        override val targetNode: Joltages = machine.joltages
      }

      BFS.search(graphSearch).target.get._2
    }
  }

  /**
   * Solution, which finds fewest presses via an ILP problem, solved by Z3.
   */
  object Z3Part2Solution extends Part2Solution {
    override def fewestPresses(machine: Machine): Int = {
      val ctx = new Context(Map("model" -> "true").asJava)
      import ctx._
      val s = mkOptimize()

      val buttonPresses = machine.buttons.zipWithIndex.map((_, i) => mkIntConst(s"x$i"))
      for (presses <- buttonPresses)
        s.Add(mkGe(presses, mkInt(0)))

      val totalPresses = buttonPresses.foldLeft[ArithExpr[IntSort]](mkInt(0))(mkAdd(_, _))
      s.MkMinimize(totalPresses)

      (machine.buttons lazyZip buttonPresses)
        .foldLeft(machine.joltages.map[ArithExpr[IntSort]](mkInt))({ case (acc, (button, presses)) =>
          button.foldLeft(acc)((acc, i) => acc.updated(i, mkSub(acc(i), presses)))
        })
        .foreach(joltageLeft =>
          s.Add(mkEq(joltageLeft, mkInt(0)))
        )

      assert(s.Check() == Status.SATISFIABLE)
      s.getModel.evaluate(totalPresses, false).toString.toInt
    }
  }

  object GaussianEliminationPart2Solution extends Part2Solution {
    /*
       x0  x1    x2  x3    x4    x5
       (3) (1,3) (2) (2,3) (0,2) (0,1)
    0:                     x4    x5    = 3
    1:     x1                    x5    = 5
    2:           x2  x3    x4          = 4
    3: x0  x1        x3                = 7


    0   0   0   0   1   1 | 3
    0   1   0   0   0   1 | 5
    0   0   1   1   1   0 | 4
    1   1   0   1   0   0 | 7

    1   1   0   1   0   0 | 7
    0   1   0   0   0   1 | 5
    0   0   1   1   1   0 | 4
    0   0   0   0   1   1 | 3

    1   0   0   1   0  -1 | 2
    0   1   0   0   0   1 | 5
    0   0   1   1   0  -1 | 1
    0   0   0   0   1   1 | 3

    1   1   1   2   1   0 | 11
               -1       1 | ?
    */

    override def fewestPresses(machine: Machine): Int = {
      val zeroCol = machine.joltages.map(_ => 0L)
      val rows =
        machine.buttons
          .map(button =>
            button.foldLeft(zeroCol)((acc, i) => acc.updated(i, 1L))
          )
          .transpose
          .zip(machine.joltages.map(_.toLong))

      val m: mutable.ArraySeq[mutable.ArraySeq[Long]] = rows.map((a, b) => (a :+ b).to(mutable.ArraySeq)).to(mutable.ArraySeq)

      def swapRows(y1: Int, y2: Int): Unit = {
        val row1 = m(y1)
        m(y1) = m(y2)
        m(y2) = row1
      }

      def multiplyRow(y: Int, factor: Long): Unit = {
        for (x2 <- 0 until (machine.buttons.size + 1))
          m(y)(x2) *= factor.toLong
      }

      def reduceDown(x: Int, y1: Int, y2: Int): Unit = {
        val c1 = m(y1)(x)
        assert(c1 > 0)
        val c2 = m(y2)(x)
        val cd = NumberTheory.lcm(c1, c2.abs)
        multiplyRow(y1, cd / c1)
        multiplyRow(y2, cd / c2)
        val factor = m(y2)(x) / m(y1)(x)
        for (x2 <- x until (machine.buttons.size + 1))
          m(y2)(x2) -= factor * m(y1)(x2)
      }

      def reduceUp(x: Int, y1: Int, y2: Int): Unit = {
        val factor = m(y2)(x) / m(y1)(x)
        for (x2 <- 0 until (machine.buttons.size + 1)) // TODO: enough to also start from x? (before zeros anyway)
          m(y2)(x2) -= factor * m(y1)(x2)
      }

      var y = 0
      for (x <- machine.buttons.indices) {
        val y2opt = m.indices.find(y2 => y2 >= y && m(y2)(x) != 0)
        y2opt match {
          case None => // move to next x
          case Some(y2) =>
            swapRows(y, y2)
            multiplyRow(y, m(y)(x).sign) // make leading coeff positive
            //assert(m(y)(x).abs == 1) // TODO: this will probably change

            for (y3 <- (y + 1) until m.size) {
              if (m(y3)(x) != 0)
                reduceDown(x, y, y3)
            }

            y += 1
        }
      }

      // check consistency
      for (y2 <- y until m.size)
        assert(m(y2).last == 0)

      val mainVars = mutable.ArrayBuffer.empty[Int]
      val freeVars = mutable.ArrayBuffer.empty[Int]
      y = 0
      for (x <- machine.buttons.indices) {
        if (y < m.size) { // TODO: break if y too big
          if (m(y)(x) == 0) {
            freeVars += x
            ()
          } // move to next x
          else {
            mainVars += x
            multiplyRow(y, m(y)(x).sign) // make leading coeff positive
            for (y3 <- 0 until y) {
              if (m(y3)(x) != 0)
                //reduceUp(x, y, y3)
                reduceDown(x, y, y3)
            }

            y += 1
          }
        }
        else
          freeVars += x // can't break if this is here
      }

      //val mSum = m.transpose.map(_.sum) // TODO: use?

      def helper0(sum: Int, len: Int): Iterator[List[Int]] = {
        if (len == 0)
          Iterator(Nil)
        else if (len == 1)
          Iterator(List(sum))
        else {
          for {
            x <- (0 to sum).iterator
            rest <- helper0(sum - x, len - 1)
          } yield x :: rest
        }
      }

      def eval(freeVals: List[Int]): List[Long] = {
        val mainVals = mainVars.view.zipWithIndex.map((mainVar, y) => {
          val row = m(y)
          val r = row.last - (freeVars lazyZip freeVals).map((freeVar, freeVal) => row(freeVar) * freeVal).sum
          if (r % row(mainVar) == 0)
            r / row(mainVar)
          else
            -1
        }).toList
        mainVals
      }

      // TODO: avoid double search...
      val bound =
        Iterator.from(0)
          .flatMap(helper0(_, freeVars.size))
          .map(freeVals => (eval(freeVals), freeVals))
          .filter(_._1.forall(_ >= 0)) // all main vals must be non-negative
          .map((s1, s2) => s1.sum + s2.sum)
          .head
      val choices = (0 to bound.toInt).iterator.flatMap(helper0(_, freeVars.size))

      val answer =
        choices
          .map(freeVals => (eval(freeVals), freeVals))
          //.take(1000) // TODO: when to stop?
          //.tapEach(println)
          .filter(_._1.forall(_ >= 0)) // all main vals must be non-negative
          .map((s1, s2) => s1.sum + s2.sum)
          .min // TODO: wrong, freeVals sum is minimal, but mainVals sum isn't

      println(answer)
      answer.toInt
    }
  }

  def parseMachine(s: String): Machine = s match {
    case s"[$lightsStr] $buttonsStr {$joltagesStr}" =>
      val lights = lightsStr.map(_ == '#').toVector
      val buttons = buttonsStr.split(" ").map(_.tail.init.split(",").map(_.toInt).toSeq).toSeq
      val joltages = joltagesStr.split(",").map(_.toInt).toVector
      Machine(lights, buttons, joltages)
  }

  def parseMachines(input: String): Seq[Machine] = input.linesIterator.map(parseMachine).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumFewestPresses(parseMachines(input)))
    println(Z3Part2Solution.sumFewestPresses(parseMachines(input)))
  }
}
