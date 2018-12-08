package eu.sim642.adventofcode2018

object Day8 {

  case class Tree(childen: Seq[Tree], metadata: Seq[Int])

  def metadataSum(tree: Tree): Int = tree.childen.map(metadataSum).sum + tree.metadata.sum

  def value(tree: Tree): Int = {
    if (tree.childen.isEmpty)
      tree.metadata.sum
    else {
      tree.metadata
        .filter(i => 1 <= i && i <= tree.childen.length)
        .map(i => tree.childen(i - 1))
        .map(value)
        .sum
    }
  }

  def parseTree(seq: List[Int]): Tree = {
    def helper(seq: List[Int]): (Tree, List[Int]) = seq match {
      case childrenCount :: metadataCount :: tl =>
        var seq = tl
        // TODO: fix this stateful fill
        val children: Seq[Tree] = Seq.fill(childrenCount)({
          val (child, seq2) = helper(seq)
          seq = seq2
          child
        })
        val (metadata, seq2) = seq.splitAt(metadataCount)
        (Tree(children, metadata), seq2)
    }

    helper(seq)._1
  }

  def parseTree(input: String): Tree = parseTree(input.split(" ").map(_.toInt).toList)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(metadataSum(parseTree(input)))
    println(value(parseTree(input)))
  }
}
