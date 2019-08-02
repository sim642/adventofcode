package eu.sim642.adventofcode2017

import scala.collection.AbstractIterator

object Day24 {

  case class Component(a: Int, b: Int) {
    def contains(i: Int): Boolean = a == i || b == i

    def other(i: Int): Int = if (a == i) b else a

    def strength: Int = a + b
  }

  private val componentRegex = """(\d+)/(\d+)""".r

  def parseComponent(str: String): Component = str match {
    case componentRegex(a, b) => Component(a.toInt, b.toInt)
  }

  type Components = Set[Component]

  def parseComponents(input: String): Components = input.linesIterator.map(parseComponent).toSet

  type Bridge = Seq[Component]

  implicit class BridgeOps(bridge: Bridge) {
    def strength: Int = bridge.map(_.strength).sum
  }

  // Scala 2.13 regression workaround: https://github.com/scala/bug/issues/11630
  implicit class IteratorFlatMap[A](it: Iterator[A]) {
    // copied from Scala 2.12
    def oldFlatMap[B](f: A => IterableOnce[B]): Iterator[B] = new AbstractIterator[B] {
      private var cur: Iterator[B] = Iterator.empty
      private def nextCur(): Unit = { cur = f(it.next()).iterator }
      def hasNext: Boolean = {
        // Equivalent to cur.hasNext || self.hasNext && { nextCur(); hasNext }
        // but slightly shorter bytecode (better JVM inlining!)
        while (!cur.hasNext) {
          if (!it.hasNext) return false
          nextCur()
        }
        true
      }
      def next(): B = (if (hasNext) cur else Iterator.empty).next()
    }

    // copied from https://github.com/scala/scala/pull/8220
    // TODO: remove in Scala 2.13.1
    def newFlatMap[B](f: A => IterableOnce[B]): Iterator[B] = new AbstractIterator[B] {
      private[this] var cur: Iterator[B] = Iterator.empty
      /** Trillium logic boolean: -1 = unknown, 0 = false, 1 = true */
      private[this] var _hasNext: Int = -1

      private[this] def nextCur(): Unit = {
        cur = null
        cur = f(it.next()).iterator
        _hasNext = -1
      }

      def hasNext: Boolean = {
        if (_hasNext == -1) {
          while (!cur.hasNext) {
            if (!it.hasNext) {
              _hasNext = 0
              // since we know we are exhausted, we can release cur for gc, and as well replace with
              // static Iterator.empty which will support efficient subsequent `hasNext`/`next` calls
              cur = Iterator.empty
              return false
            }
            nextCur()
          }
          _hasNext = 1
          true
        } else _hasNext == 1
      }
      def next(): B = {
        if (hasNext) {
          _hasNext = -1
        }
        cur.next()
      }
    }
  }

  def validBridges(components: Components): Iterator[Bridge] = {

    def helper(components: Components, port: Int): Iterator[Bridge] = {
      val portComponents = components.filter(_.contains(port))
      portComponents.iterator
        .newFlatMap { component =>
          val newPort = component.other(port)

          (Iterator.single(Nil) ++ helper(components - component, newPort))
            .map(bridge => component +: bridge)
        }
    }

    helper(components, 0)
  }

  def validLongBridges(components: Components): Iterator[Bridge] = {

    def helper(components: Components, port: Int): Iterator[Bridge] = {
      val portComponents = components.filter(_.contains(port))
      if (portComponents.isEmpty)
        Iterator.single(Nil)
      else
        portComponents.iterator
          .newFlatMap { component =>
            val newPort = component.other(port)

            helper(components - component, newPort)
              .map(bridge => component +: bridge)
          }
    }

    helper(components, 0)
  }

  def strongestBridgeStrength(components: Components): Int = validLongBridges(components).map(_.strength).max

  def strongestBridgeStrength(input: String): Int = strongestBridgeStrength(parseComponents(input))


  def validLongestBridges(components: Components): Iterator[Bridge] = {
    val bridges = validLongBridges(components).toSet
    val maxLength = bridges.map(_.length).max
    bridges.filter(_.length == maxLength).iterator
  }

  def longestBridgeStrength(components: Components): Int = validLongestBridges(components).map(_.strength).max

  def longestBridgeStrength(input: String): Int = longestBridgeStrength(parseComponents(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(strongestBridgeStrength(input))
    println(longestBridgeStrength(input))
  }
}
