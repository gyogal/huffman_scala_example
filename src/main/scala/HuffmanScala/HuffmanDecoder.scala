package HuffmanScala

import scala.annotation.tailrec

object HuffmanDecoder {

  def decode(input: String, dictionary: Map[Char, String]): String = {
    val decodeDictionary = dictionary.map(_.swap)
    decodeString(input, decodeDictionary)
  }

  @tailrec
  private def getNextSymbolLength(length: Int, encodedString: String, dict: Map[String, Char]): Option[Int] = {
    if (encodedString == "") None
    else if (!encodedString.isDefinedAt(length - 1)) throw new RuntimeException("Unknown symbol found")
    else if (dict.contains(encodedString.take(length))) Some(length)
    else getNextSymbolLength(length + 1, encodedString, dict)
  }

  private def getNextChar(encodedString: String, dict: Map[String, Char]): (Option[(Char, String)]) = {
    val length = getNextSymbolLength(1, encodedString, dict)
    length.map(l =>
      (dict.getOrElse(encodedString.take(l), throw new RuntimeException(s"Unable to decode symbol $encodedString length $l")),
        encodedString.substring(l)))
  }

  @tailrec
  private def decodeString(encodedString: String, dict: Map[String, Char], decodedPart: String = ""): String = {
    val nextChar = getNextChar(encodedString, dict)
    nextChar match {
      case Some((c: Char, stringWithoutDecodedChar: String)) => decodeString(stringWithoutDecodedChar, dict, decodedPart + c)
      case None => decodedPart
    }
  }
}
