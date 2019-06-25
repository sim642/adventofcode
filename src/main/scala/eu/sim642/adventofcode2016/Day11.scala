package eu.sim642.adventofcode2016

object Day11 {

  sealed trait Object {
    val element: String
  }
  case class Microchip(element: String) extends Object
  case class Generator(element: String) extends Object

  case class State(floorObjects: Vector[Set[Object]], elevatorFloor: Int) {
    assert(floorObjects.size == 4)

    def steps: Iterator[State] = {
      def isValid(objects: Set[Object]): Boolean = {
        val hasGeneratorlessMicrochips = objects.exists({
          case Microchip(element) => !objects.contains(Generator(element))
          case Generator(element) => false
        })
        val hasGenerator = objects.exists({
          case Microchip(element) => false
          case Generator(element) => true
        })
        !(hasGenerator && hasGeneratorlessMicrochips)
      }

      val floorDeltas = {
        if (floorObjects.view(0, elevatorFloor).forall(_.isEmpty))
          Iterator(1)
        else
          Iterator(1, -1)
      }

      for {
        floorDelta <- floorDeltas
        newElevatorFloor = elevatorFloor + floorDelta
        if newElevatorFloor >= 0 && newElevatorFloor < 4
        currentObjects = floorObjects(elevatorFloor)
        moveObjects <- currentObjects.subsets(1) ++ currentObjects.subsets(2)
        newCurrentObjects = currentObjects -- moveObjects
        if isValid(newCurrentObjects)
        newNewObjects = floorObjects(newElevatorFloor) ++ moveObjects
        if isValid(newNewObjects)
      } yield State(floorObjects.updated(elevatorFloor, newCurrentObjects).updated(newElevatorFloor, newNewObjects), newElevatorFloor)
    }
  }

  def isFinal(state: State): Boolean = state.floorObjects.view(0, 3).forall(_.isEmpty)

  // bfs adapted from 2018 Day 20
  def solveSteps(visited: Set[State], toVisit: Set[State], steps: Int): Int = {
    if (toVisit.exists(isFinal))
      steps
    else {
      val neighbors = for {
        state <- toVisit.iterator
        newState <- state.steps
      } yield newState
      val newVisited = visited ++ toVisit
      val newToVisit = neighbors.filter(!visited.contains(_)) // more efficient than -- because visited is large
      solveSteps(newVisited, newToVisit.toSet, steps + 1)
    }
  }

  def solveSteps(initialState: State): Int = solveSteps(Set.empty, Set(initialState), 0)

  def solveStepsExtra(initialState: State): Int = {
    val newInitialState = initialState.copy(floorObjects = initialState.floorObjects.updated(0, initialState.floorObjects(0) ++ Set(
      Generator("elerium"),
      Microchip("elerium"),
      Generator("dilithium"),
      Microchip("dilithium"),
    )))

    solveSteps(newInitialState)
  }

  private val objectRegex = """(\w+)-compatible microchip|(\w+) generator""".r

  def parseInput(input: String): State = {
    val floorObjects: Vector[Set[Object]] = input.lines.take(3).map({ line =>
      objectRegex.findAllMatchIn(line).map({ m =>
        if (m.group(1) != null)
          Microchip(m.group(1))
        else if (m.group(2) != null)
          Generator(m.group(2))
        else
          ???.asInstanceOf[Object] // TODO: why is this needed?
      }).toSet
    }).toVector :+ Set()

    State(floorObjects, 0)
  }

  def solveSteps(input: String): Int = solveSteps(parseInput(input))

  def solveStepsExtra(input: String): Int = solveStepsExtra(parseInput(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(solveSteps(input))
    println(solveStepsExtra(input)) // TODO: optimize a lot
  }
}
