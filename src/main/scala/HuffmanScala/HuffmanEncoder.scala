package HuffmanScala

import scala.collection.mutable.PriorityQueue

object HuffmanEncoder {
  type Freq = (Char, Int)

  implicit object ReverseTreeComparer extends Ordering[BinaryTree[Freq]] {
    override def compare(x: BinaryTree[Freq], y: BinaryTree[Freq]): Int = y.data._2.compare(x.data._2)
  }

  private def getFrequencies(input: String): Map[Char, Int] = {
    input.foldLeft(Map.empty[Char, Int])((f, c) => f + ((c, f.getOrElse(c, 0) + 1)))
  }
  def encode(input: String): (String, Map[Char, String]) = {
    val frequencies: Map[Char, Int] = getFrequencies(input)

    def buildHuffmanTree(frequencies: Map[Char, Int]): BinaryTree[Freq] = {
      val reversePriorityQueue: PriorityQueue[BinaryTree[Freq]] = PriorityQueue.empty ++
        frequencies.map { case (c, f) => LeafNode((c, f)) }
      while (reversePriorityQueue.length > 1) {
        val a = reversePriorityQueue.dequeue()
        val b = reversePriorityQueue.dequeue()
        reversePriorityQueue.enqueue(NonLeafNode(('x', a.data._2 + b.data._2), a, b))
      }
      reversePriorityQueue.dequeue()
    }

    def translateToBinaryCodes(t: BinaryTree[Freq], path: String = ""): Map[Char, String] = {
      t match {
        case LeafNode((c, f)) if path == "" => Map(c -> "0")
        case LeafNode((c, f)) => Map(c -> path)
        case NonLeafNode((c, f), left, right) =>
          translateToBinaryCodes(left, path + "0") ++ translateToBinaryCodes(right, path + "1")
      }
    }

    val huffmanTable = buildHuffmanTree(frequencies)
    val binaryCodes = translateToBinaryCodes(huffmanTable)

    (input.map(c => binaryCodes(c)).reduce(_ ++ _), binaryCodes)
  }
}
