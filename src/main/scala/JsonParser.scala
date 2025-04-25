import scala.util.parsing.combinator.*

object JsonParser extends RegexParsers {
  override def skipWhitespace = true

  private def value: Parser[Any] = obj | arr | stringLiteral |
    floatingPointNumber ^^ (num => if (num.contains(".")) num.toDouble else num.toLong) |
    "null" ^^^ null |
    "true" ^^^ true |
    "false" ^^^ false

  private def floatingPointNumber: Parser[String] =
    regex("""[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?""".r)

  private def obj: Parser[Map[String, Any]] =
    "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)

  private def member: Parser[(String, Any)] =
    stringLiteral ~ (":" ~> value) ^^ { case name ~ value => (name, value) }

  private def arr: Parser[List[Any]] =
    "[" ~> repsep(value, ",") <~ "]"

  private def stringLiteral: Parser[String] =
    "\"" ~> regex("""(\\.|[^"\\])*""".r) <~ "\"" ^^ { str => unescape(str) }

  private def unescape(str: String): String = {
    val buf = new StringBuilder
    var i = 0
    while (i < str.length) {
      if (str.charAt(i) == '\\' && i + 1 < str.length) {
        str.charAt(i + 1) match {
          case '"' => buf.append('"'); i += 2
          case '\\' => buf.append('\\'); i += 2
          case '/' => buf.append('/'); i += 2
          case 'b' => buf.append('\b'); i += 2
          case 'f' => buf.append('\f'); i += 2
          case 'n' => buf.append('\n'); i += 2
          case 'r' => buf.append('\r'); i += 2
          case 't' => buf.append('\t'); i += 2
          case 'u' if i + 5 < str.length =>
            // Handle unicode escapes
            val hexCode = str.substring(i + 2, i + 6)
            buf.append(Integer.parseInt(hexCode, 16).toChar)
            i += 6
          case _ => buf.append(str.charAt(i)); i += 1
        }
      } else {
        buf.append(str.charAt(i))
        i += 1
      }
    }
    buf.toString
  }

  def parseJson(input: String): Either[String, Any] = {
    parse(value, input) match {
      case Success(result, _) => Right(result)
      case Failure(msg, _) => Left(s"Parsing failure: $msg")
      case Error(msg, _) => Left(s"Parsing error: $msg")
    }
  }
}

object JsonTest {
  def main(args: Array[String]): Unit = {
    val jsonString =
      """
    {
      "name": "John Doe",
      "age": 32,
      "isEmployed": true,
      "escaped": "test\u1234",
      "address": {
        "street": "123 Main St",
        "city": "Anytown"
      },
      "phoneNumbers": [
        "555-1234",
        "555-5678"
      ],
      "score": 97.5,
      "nullValue": null
    }
    """

    JsonParser.parseJson(jsonString) match {
      case Right(result) => println(s"Parsed successfully: $result")
      case Left(error) => println(s"Failed to parse: $error")
    }
  }
}