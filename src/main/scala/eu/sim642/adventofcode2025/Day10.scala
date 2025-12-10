package eu.sim642.adventofcode2025

import com.microsoft.z3.{ArithExpr, Context, IntExpr, IntSort, Status}
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, TargetNode, UnitNeighbors}

import scala.jdk.CollectionConverters.*

object Day10 {

  type Lights = Vector[Boolean]
  type Buttons = Seq[Seq[Int]]
  type Joltages = Vector[Int]

  case class Machine(lights: Lights, buttons: Buttons, joltages: Joltages)

  def fewestPresses(machine: Machine): Int = {
    val graphSearch = new GraphSearch[Lights] with UnitNeighbors[Lights] with TargetNode[Lights] {
      override val startNode: Lights = machine.lights.map(_ => false)

      override def unitNeighbors(lights: Lights): IterableOnce[Lights] =
        machine.buttons.map(_.foldLeft(lights)((acc, i) => acc.updated(i, !acc(i))))

      override val targetNode: Lights = machine.lights
    }

    BFS.search(graphSearch).target.get._2
  }

  def sumFewestPresses(machines: Seq[Machine]): Int = machines.map(fewestPresses).sum

  /*
     x0  x1    x2  x3    x4    x5
     (3) (1,3) (2) (2,3) (0,2) (0,1)
  0:                     x4    x5    = 3
  1:     x1                    x5    = 5
  2:           x2  x3    x4          = 4
  3: x0            x3                = 7

   */

  // TODO: optimize
  def fewestPresses2(machine: Machine): Int = {
    /*val graphSearch = new GraphSearch[Joltages] with UnitNeighbors[Joltages] with TargetNode[Joltages] {
      override val startNode: Joltages = machine.joltages.map(_ => 0)

      override def unitNeighbors(joltages: Joltages): IterableOnce[Joltages] =
        machine.buttons.map(_.foldLeft(joltages)((acc, i) => acc.updated(i, acc(i) + 1)))

      override val targetNode: Joltages = machine.joltages
    }

    BFS.search(graphSearch).target.get._2*/

    val ctx = new Context(Map("model" -> "true").asJava)
    import ctx._
    val s = mkOptimize()

    val buttonVars = machine.buttons.zipWithIndex.map((_, i) => mkIntConst(s"x$i"))
    val lhss =
      machine.buttons
        .lazyZip(buttonVars)
        .foldLeft(machine.joltages.map[ArithExpr[IntSort]](i => mkInt(i)))({ case (accs, (button, buttonVar)) =>
          button.foldLeft(accs)((accs, i) => accs.updated(i, mkSub(accs(i), buttonVar)))
        })

    for (lhs <- lhss)
      s.Add(mkEq(lhs, mkInt(0)))

    for (v <- buttonVars)
      s.Add(mkGe(v, mkInt(0)))

    val presses = buttonVars.foldLeft[ArithExpr[IntSort]](mkInt(0))((acc, v) => mkAdd(acc, v))
    s.MkMinimize(presses)
    assert(s.Check() == Status.SATISFIABLE)
    //println(s.getModel)
    s.getModel.evaluate(presses, false).toString.toInt
  }

  def sumFewestPresses2(machines: Seq[Machine]): Int = machines.map(fewestPresses2).tapEach(println).sum

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
    println(sumFewestPresses(parseMachines(input)))
    println(sumFewestPresses2(parseMachines(input)))
  }
}
