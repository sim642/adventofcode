package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.NumberTheory

import scala.collection.{View, mutable}

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

  case class Conjunction(remember: Map[String, Pulse] = Map.empty) extends Module { // TODO: add initial Lows
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
      val mCircuit = modules.to(mutable.Map)
      val b = Seq.newBuilder[(String, Pulse, String)]

      val queue = mutable.Queue.empty[(String, Pulse, String)]
      queue.enqueue(("button", Pulse.Low, "broadcaster"))

      while (queue.nonEmpty) {
        val s@(from, pulse, to) = queue.dequeue()
        //println(s)
        b += s

        mCircuit.get(to) match {
          case None => () // output
          case Some(module) =>
            val outputs = this.outputs(to)
            val (newModule, outPulse) = module.handle(from, pulse)
            mCircuit(to) = newModule

            for {
              pulse <- outPulse
              output <- outputs
            } queue.enqueue((to, pulse, output))
        }
      }

      val newCircuit = copy(modules = mCircuit.toMap)
      (newCircuit, b.result())
    }
  }

  def countPulses(circuit: Circuit): Int = {
    var mCircuit = circuit

    var lowPulses = 0
    var highPulses = 0

    for (i <- 0 until 1000) {
      val (mCircuit2, pulses) = mCircuit.simulate()
      mCircuit = mCircuit2

      for ((_, pulse, _) <- pulses) {
        pulse match {
          case Pulse.Low => lowPulses += 1
          case Pulse.High => highPulses += 1
        }
      }
    }

    lowPulses * highPulses
  }

  def countRx(circuit: Circuit): Long = {
    val ms = circuit.inputs(circuit.inputs("rx").head)

    var mCircuit = circuit

    var presses = 0

    val found = mutable.Map.empty[String, Int]

    while (found.size < ms.size) {
      val (mCircuit2, pulses) = mCircuit.simulate()
      mCircuit = mCircuit2
      presses += 1

      for (pulse <- pulses) {
        pulse match {
          case (from, Pulse.High, to) if ms(from) =>
            found(from) = presses
          case _ => ()
        }
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
      (module, outputs) <- outputs.view // TODO: why need view?
      output <- outputs
    } yield module -> output).groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap

    val initializedModules = modules.map({
      case (module, Conjunction(_)) =>
        module -> Conjunction(inputs(module).map(_ -> Pulse.Low).toMap)
      case other => other
    })

    Circuit(initializedModules, outputs, inputs)
  }

  def printDot(circuit: Circuit): Unit = {
    //println("digraph G {")
    //for ((module, (m, outputs)) <- circuit) {
    //  for (output <- outputs)
    //    println(s"$module -> $output;")
    //}
    //println("}")
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    //println(countPulses(parseCircuit(input)))
    //printDot(parseCircuit(input))
    println(countRx(parseCircuit(input))) // 217317393039529

    //high from xr after 3769
    //high from vt after 3797
    //high from fv after 3863
    //high from kk after 3931
  }
}
