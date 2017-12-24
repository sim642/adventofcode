package eu.sim642.adventofcode2017

object Day24 {

  case class Component(a: Int, b: Int) {

    def contains(i: Int): Boolean = a == i || b == i

    def other(i: Int): Int = if (a == i) b else a
  }

  private val componentRegex = """(\d+)/(\d+)""".r

  def parseComponent(str: String): Component = str match {
    case componentRegex(a, b) => Component(a.toInt, b.toInt)
  }

  def parseComponents(input: String): Seq[Component] = input.lines.map(parseComponent).toSeq

  type Bridge = Seq[Component]

  def validBridges(components: Seq[Component]): Set[Bridge] = {

    def helper(components: Seq[Component], port: Int): Set[Bridge] = {
      val portComponents = components.filter(_.contains(port))
      (for {
        component <- portComponents
        newPort = component.other(port)
        bridge <- Set(Seq()) ++ helper(components.diff(Seq(component)), newPort)
      } yield component +: bridge).toSet
    }

    helper(components, 0)
  }

  def validLongBridges(components: Seq[Component]): Set[Bridge] = {

    def helper(components: Seq[Component], port: Int): Set[Bridge] = {
      val portComponents = components.filter(_.contains(port))
      if (portComponents.isEmpty)
        Set(Seq())
      else
        (for {
          component <- portComponents
          newPort = component.other(port)
          bridge <- helper(components.diff(Seq(component)), newPort)
        } yield component +: bridge).toSet
    }

    helper(components, 0)
  }

  def strongestBridgeStrength(components: Seq[Component]): Int = validLongBridges(components).map(_.map(c => c.a + c.b).sum).max

  def strongestBridgeStrength(input: String): Int = strongestBridgeStrength(parseComponents(input))


  def longestBridgeStrength(components: Seq[Component]): Int = {
    val bridges = validLongBridges(components)
    val maxLength = bridges.map(_.length).max
    bridges.filter(_.length == maxLength).map(_.map(c => c.a + c.b).sum).max
  }

  def longestBridgeStrength(input: String): Int = longestBridgeStrength(parseComponents(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(strongestBridgeStrength(input))
    println(longestBridgeStrength(input))
  }
}
