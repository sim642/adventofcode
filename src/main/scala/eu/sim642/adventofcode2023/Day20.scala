package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.NumberTheory

import scala.collection.mutable

object Day20 {

  enum Pulse {
    case Low
    case High
  }

  sealed trait Module {
    def handle(from: String, pulse: Pulse): (Module, Option[Pulse])
  }

  case class FlipFlop(state: Boolean = false) extends Module {
    override def handle(from: String, pulse: Pulse): (Module, Option[Pulse]) = pulse match {
      case Pulse.High => (this, None)
      case Pulse.Low => (FlipFlop(!state), Some(if (!state) Pulse.High else Pulse.Low))
    }
  }

  case class Conjunction(remember: Map[String, Pulse] = Map.empty) extends Module {
    override def handle(from: String, pulse: Pulse): (Module, Option[Pulse]) = {
      val newRemember = remember.updated(from, pulse)
      val outPulse = if (newRemember.forall(_._2 == Pulse.High)) Pulse.Low else Pulse.High
      (Conjunction(newRemember), Some(outPulse))
    }
  }

  case object Broadcast extends Module {
    override def handle(from: String, pulse: Pulse): (Module, Option[Pulse]) =
      (this, Some(pulse))
  }

  case class Circuit(modules: Map[String, Module],
                     outputs: Map[String, Seq[String]],
                     inputs: Map[String, Set[String]]) {

    def simulate(): (Circuit, Seq[(String, Pulse, String)]) = {
      val currentModules = modules.to(mutable.Map)
      val pulses = Seq.newBuilder[(String, Pulse, String)]

      val queue = mutable.Queue.empty[(String, Pulse, String)]
      queue.enqueue(("button", Pulse.Low, "broadcaster"))

      while (queue.nonEmpty) {
        val pulseTriple@(from, pulse, to) = queue.dequeue()
        pulses += pulseTriple

        currentModules.get(to) match {
          case None => () // output
          case Some(module) =>
            val toOutputs = outputs(to)
            val (newModule, outPulse) = module.handle(from, pulse)
            currentModules(to) = newModule

            for {
              pulse <- outPulse
              toOutput <- toOutputs
            } queue.enqueue((to, pulse, toOutput))
        }
      }

      val newCircuit = copy(modules = currentModules.toMap)
      (newCircuit, pulses.result())
    }
  }

  def countLowHigh(initialCircuit: Circuit): Int = {
    var circuit = initialCircuit
    val pulseCount = mutable.Map.empty[Pulse, Int].withDefaultValue(0)

    for (_ <- 0 until 1000) {
      val (newCircuit, pulses) = circuit.simulate()
      circuit = newCircuit

      for ((_, pulse, _) <- pulses)
        pulseCount(pulse) += 1
    }

    pulseCount.values.product
  }

  def countRx(initialCircuit: Circuit): Long = {
    var circuit = initialCircuit
    val rxInputs = circuit.inputs("rx")
    assert(rxInputs.size == 1)
    val todo = circuit.inputs(rxInputs.head).to(mutable.Set)
    assert(todo.size == 4)
    val found = mutable.Map.empty[String, Int]

    var i = 0
    while (todo.nonEmpty) {
      val (newCircuit, pulses) = circuit.simulate()
      circuit = newCircuit
      i += 1

      for (case (from, Pulse.High, _) <- pulses if todo(from)) {
        todo -= from
        found(from) = i
      }
    }

    NumberTheory.lcm(found.values.map(_.toLong).toSeq)
  }


  def parseModule(s: String): (String, (Module, Seq[String])) = s match {
    case s"$moduleStr -> $outputsStr" =>
      val (name, module) = moduleStr match {
        case s"broadcaster" => ("broadcaster", Broadcast)
        case s"%$name" => (name, FlipFlop())
        case s"&$name" => (name, Conjunction())
      }
      val outputs = outputsStr.split(", ").toSeq
      (name, (module, outputs))
  }

  def parseCircuit(input: String): Circuit = {
    val moduleOutputs = input.linesIterator.map(parseModule).toMap
    val modules = moduleOutputs.view.mapValues(_._1).toMap
    val outputs = moduleOutputs.view.mapValues(_._2).toMap

    val inputs = (for {
      (module, outputs) <- outputs.iterator
      output <- outputs
    } yield module -> output).groupMapReduce(_._2)(p => Set(p._1))(_ ++ _)

    val initializedModules = modules.map({
      case (module, Conjunction(_)) =>
        module -> Conjunction(inputs(module).map(_ -> Pulse.Low).toMap)
      case other => other
    })

    Circuit(initializedModules, outputs, inputs)
  }

  def printCircuitDot(circuit: Circuit): Unit = {
    println("digraph circuit {")
    for ((module, outputs) <- circuit.outputs) {
      for (output <- outputs)
        println(s"  $module -> $output;")
    }
    println("}")
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val circuit = parseCircuit(input)
    println(countLowHigh(circuit))
    //printCircuitDot(circuit)
    println(countRx(circuit))
  }
}
