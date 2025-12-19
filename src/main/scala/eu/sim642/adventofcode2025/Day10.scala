package eu.sim642.adventofcode2025

import com.microsoft.z3.{ArithExpr, Context, IntSort, Status}
import eu.sim642.adventofcodelib.GaussianElimination
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, TargetNode, UnitNeighbors}

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
      import ctx.*
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

  /**
   * Solution, which performs Gaussian elimination and then brute forces free variables in their ranges.
   */
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
      val zeroCol = machine.joltages.map(_ => 0)
      val rows =
        machine.buttons
          .map(_.foldLeft(zeroCol)(_.updated(_, 1)))
          .transpose

      val sol = GaussianElimination.solve(rows, machine.joltages).get
      //val mSum = m.transpose.map(_.sum) // TODO: use?

      def helper(freeMaxs: List[Int]): Iterator[List[Int]] = freeMaxs match { // TODO: this seems like it should exist from earlier somewhere
        case Nil => Iterator(Nil)
        case freeMax :: newFreeMaxs =>
          for {
            freeVal <- (0 to freeMax).iterator
            newFreeVals <- helper(newFreeMaxs)
          } yield freeVal :: newFreeVals
      }

      val maxs = machine.buttons.map(_.map(machine.joltages).min)
      val freeMaxs = sol.freeVars.map(maxs)
      val dependentMaxs = sol.dependentVars.map(maxs)
      (for {
        freeVals <- helper(freeMaxs.toList)
        dependentVals <- sol.evaluate(freeVals)
        if dependentVals.forall(_ >= 0)
        if (dependentVals lazyZip dependentMaxs).forall(_ <= _)
      } yield dependentVals.sum + freeVals.sum).min
    }
  }

  /**
   * Solution, which reduces parity of joltages to part 1-like problem.
   * @see [[https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/]]
   */
  object BifurcatePart2Solution extends Part2Solution {
    override def fewestPresses(machine: Machine): Int = {
      def parity(joltages: Joltages): Lights =
        joltages.map(i => if (i % 2 != 0) true else false)

      val zeroJoltages: Joltages = machine.joltages.map(_ => 0)
      val parityPresses =
        machine.buttons.toSet
          .subsets()
          .map(buttons =>
            buttons.foldLeft(zeroJoltages)((acc, button) =>
              button.foldLeft(acc)((acc, i) => acc.updated(i, acc(i) + 1))
            ) -> buttons.size
          )
          .toSeq
          .groupBy((joltages, _) => parity(joltages))
          .withDefaultValue(Seq.empty)

      val memo = mutable.Map.empty[Joltages, Option[Int]] // memoization is a bonus, but not strictly necessary

      def helper(joltages: Joltages): Option[Int] = {
        memo.getOrElseUpdate(joltages, {
          if (joltages == zeroJoltages)
            Some(0)
          else if (joltages.exists(_ < 0))
            None
          else {
            (for {
              (pressJoltages, presses) <- parityPresses(parity(joltages))
              newJoltages = (joltages lazyZip pressJoltages).map(_ - _).map(_ / 2)
              newPresses <- helper(newJoltages)
            } yield 2 * newPresses + presses).minOption
          }
        })
      }

      helper(machine.joltages).get
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
    println(BifurcatePart2Solution.sumFewestPresses(parseMachines(input)))
  }
}
