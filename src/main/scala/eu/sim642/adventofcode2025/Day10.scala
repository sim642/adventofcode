package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, TargetNode, UnitNeighbors}

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

  // TODO: optimize
  def fewestPresses2(machine: Machine): Int = {
    val graphSearch = new GraphSearch[Joltages] with UnitNeighbors[Joltages] with TargetNode[Joltages] {
      override val startNode: Joltages = machine.joltages.map(_ => 0)

      override def unitNeighbors(joltages: Joltages): IterableOnce[Joltages] =
        machine.buttons.map(_.foldLeft(joltages)((acc, i) => acc.updated(i, acc(i) + 1)))

      override val targetNode: Joltages = machine.joltages
    }

    BFS.search(graphSearch).target.get._2
  }

  def sumFewestPresses2(machines: Seq[Machine]): Int = machines.map(fewestPresses2).sum

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
