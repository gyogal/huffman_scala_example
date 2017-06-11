package HuffmanScala

import org.scalatest._

class HuffmanEncoderTest extends FlatSpec with Matchers {
  "Encoder" should "work with empty string" in {
    HuffmanEncoder.encode("") should be(("", Map.empty))
  }

  it should "assign the shortest code to the most frequent character" in {
    val result = HuffmanEncoder.encode("assdddddf1234")

    result._2.foreach {
      case (_, code) => result._2.get('d').map(dsymbol => assert(dsymbol.length <= code.length))
    }
  }

  it should "generate dictionary with a code for each unique character" in {
    val inputString = "Example string 12345"
    val result = HuffmanEncoder.encode(inputString)
    result._2.size == inputString.distinct.length
  }

  it should "generate binary string" in {
    val result = HuffmanEncoder.encode("Example text")
    result._1 should fullyMatch regex "^[01]*$"
  }
}
