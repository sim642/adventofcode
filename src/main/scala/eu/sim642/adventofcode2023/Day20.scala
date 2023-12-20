package eu.sim642.adventofcode2023

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

  type Circuit = Map[String, (Module, Seq[String])]

  def countPulses(circuit: Circuit): Int = {
    val mCircuit = circuit.to(mutable.Map)

    var lowPulses = 0
    var highPulses = 0

    for (i <- 0 until 1000) {

      val queue = mutable.Queue.empty[(String, Pulse, String)]
      queue.enqueue(("button", Pulse.Low, "broadcaster"))

      while (queue.nonEmpty) {
        val s@(from, pulse, to) = queue.dequeue()
        //println(s)

        pulse match {
          case Pulse.Low => lowPulses += 1
          case Pulse.High => highPulses += 1
        }

        mCircuit.get(to) match {
          case None => () // output
          case Some(module, outputs) =>
            val (newModule, outPulse) = module.handle(from, pulse)
            mCircuit(to) = (newModule, outputs)

            for {
              pulse <- outPulse
              output <- outputs
            } queue.enqueue((to, pulse, output))
        }
      }
    }

    println((lowPulses, highPulses))
    lowPulses * highPulses
  }

  def countRx(circuit: Circuit): Long = {
    val mCircuit = circuit.to(mutable.Map)

    var presses = 0L

    while (true) {

      val queue = mutable.Queue.empty[(String, Pulse, String)]
      queue.enqueue(("button", Pulse.Low, "broadcaster"))
      presses += 1

      while (queue.nonEmpty) {
        val s@(from, pulse, to) = queue.dequeue()
        //println(s)

        s match {
          case (_, Pulse.Low, "rx") =>
            return presses
          case (from, Pulse.High, "sq") =>
            println(s"high from $from after $presses")
          case _ => ()
        }

        mCircuit.get(to) match {
          case None => () // output
          case Some(module, outputs) =>
            val (newModule, outPulse) = module.handle(from, pulse)
            mCircuit(to) = (newModule, outputs)

            for {
              pulse <- outPulse
              output <- outputs
            } queue.enqueue((to, pulse, output))
        }
      }
    }

    ???
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

  def initializeConjunctions(circuit: Circuit): Circuit = {
    //println(circuit)
    val inputs = (for {
      (module, (_, outputs)) <- circuit.view // TODO: why need view?
      output <- outputs
    } yield module -> output).groupMap(_._2)(_._1)
    //println(inputs)

    circuit.map({
      case (module, (Conjunction(_), outputs)) =>
        module -> (Conjunction(inputs(module).map(_ -> Pulse.Low).toMap), outputs)
      case other => other
    })
  }

  def parseCircuit(input: String): Circuit = initializeConjunctions(input.linesIterator.map(parseModule).toMap)

  def printDot(circuit: Circuit): Unit = {
    println("digraph G {")
    for ((module, (m, outputs)) <- circuit) {
      for (output <- outputs)
        println(s"$module -> $output;")
    }
    println("}")
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    //println(countPulses(parseCircuit(input)))
    //printDot(parseCircuit(input))
    println(countRx(parseCircuit(input))) // 217317393039529
  }
}
