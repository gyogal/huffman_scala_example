package HuffmanScala

object HuffmanTest {
  def main(args: Array[String]): Unit = {
    println("Simple Huffman coding example in Scala")
    print("Enter text to be encoded or press ^D: ")
    for (ln <- io.Source.stdin.getLines) {
      val (encodedText, dict) = HuffmanEncoder.encode(ln)
      println(s"Encoded test: $encodedText")
      println("Huffman table: ")
      println(dict.map { case (c: Char, s: String) => s"   $c -> $s" }.mkString("\n"))
      println(s"Decoded text: ${HuffmanDecoder.decode(encodedText, dict)}")
      print("Enter some more text to be encoded or press ^D: ")
    }
  }
}
