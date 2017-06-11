package HuffmanScala

import scala.collection.mutable

object HuffmanEncoder {
  type Freq = (Char, Int)

  implicit object ReverseTreeComparer extends Ordering[BinaryTree[Freq]] {
    override def compare(x: BinaryTree[Freq], y: BinaryTree[Freq]): Int = y.data._2.compare(x.data._2)
  }

  def encode(input: String): (String, Map[Char, String]) = {
    val frequencies: Map[Char, Int] = getFrequencies(input)
    val huffmanTable = buildHuffmanTree(frequencies)
    val binaryCodes = huffmanTable.map(translateToBinaryCodes(_)).getOrElse(Map.empty)

    (input.map(c => binaryCodes(c)).foldLeft("")(_ ++ _), binaryCodes)
  }

  private def getFrequencies(input: String): Map[Char, Int] = {
    input.foldLeft(Map.empty[Char, Int])((f, c) => f + ((c, f.getOrElse(c, 0) + 1)))
  }

  private def buildHuffmanTree(frequencies: Map[Char, Int]): Option[BinaryTree[Freq]] = {
    val reversePriorityQueue: mutable.PriorityQueue[BinaryTree[Freq]] = mutable.PriorityQueue.empty ++
      frequencies.map { case (c, f) => LeafNode((c, f)) }

    if (reversePriorityQueue.isEmpty) None
    else {
      while (reversePriorityQueue.length > 1) {
        val a = reversePriorityQueue.dequeue()
        val b = reversePriorityQueue.dequeue()
        reversePriorityQueue.enqueue(NonLeafNode(('x', a.data._2 + b.data._2), a, b))
      }
      Some(reversePriorityQueue.dequeue())
    }
  }

  private def translateToBinaryCodes(t: BinaryTree[Freq], path: String = ""): Map[Char, String] = {
    t match {
      case LeafNode((c, _)) if path == "" => Map(c -> "0")
      case LeafNode((c, _)) => Map(c -> path)
      case NonLeafNode((_, _), left, right) =>
        translateToBinaryCodes(left, path + "0") ++ translateToBinaryCodes(right, path + "1")
    }
  }
}
