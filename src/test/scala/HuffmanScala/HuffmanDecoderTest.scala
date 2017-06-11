package HuffmanScala

import org.scalatest._

class HuffmanDecoderTest extends FlatSpec with Matchers {
  "Decoder" should "work with empty string" in {
    HuffmanDecoder.decode("", Map.empty) shouldBe empty
  }

  it should "work with some simple examples" in {
    val simpleDict: Map[Char, String] = Map('a' -> "0", 'b' -> "10", 'c' -> "11")

    def translate(s: String) = s.flatMap(simpleDict.get).reduce(_ + _)

    def decode(s: String) = HuffmanDecoder.decode(s, simpleDict)

    decode(translate("a")) should be("a")
    decode(translate("abb")) should be("abb")
    decode(translate("cba")) should be("cba")
    decode(translate("cccc")) should be("cccc")
  }

  it should "throw if unknown characters are encountered" in {
    val simpleDict: Map[Char, String] = Map('a' -> "0", 'b' -> "10")
    assertThrows[Exception] {
      HuffmanDecoder.decode("010111000001010", simpleDict)
    }
  }
}
