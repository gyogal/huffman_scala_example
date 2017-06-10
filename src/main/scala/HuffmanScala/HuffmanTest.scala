package HuffmanScala

object HuffmanTest {
  def main(args: Array[String]): Unit = {
    for (ln <- io.Source.stdin.getLines) println((HuffmanDecoder.decode(_, _)).tupled(HuffmanEncoder.encode(ln)))
  }
}
