package Chapter9

import Chapter9.Json.JNumber

trait Json {
}

object Json {
    case object JNull extends Json
    case class JNumber(get: Double) extends Json
    case class JString(get: String) extends Json
    case class JBool(get: Boolean) extends Json
    case class JArray(get: IndexedSeq[Json]) extends Json
    case class JObject(get: Map[String, Json]) extends Json
}

object JsonParser {
    import Json._

    def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[Json] = {
        import P._

        val spaces: Parser[String] = char(' ').many.slice

        val nullParser: Parser[JNull.type] = string("null").flatMap(_ => succeed(JNull))
        val numParser: Parser[JNumber] = regex(raw"[^] },]".r).map(_.toDouble).map(JNumber(_))
        val stringParser: Parser[JString] = regex("\".*\"".r).map(JString(_))
        val boolParser: Parser[JBool] = ("true" | "false").map(_.toBoolean).map(JBool(_))
        val arrayParser: Parser[JArray] = ???
        val objParser: Parser[JObject] = ???


        def findParser(indicator: String): Parser[Json] = indicator(0).toLower match {
            case 'n' => nullParser
            //case raw"\d|-.*".r => numParser
            case '"' => stringParser
            case 't' | 'f' => boolParser
            case '[' => arrayParser
            case '{' => objParser
            case _ => throw new Exception("don't know what this is") // todo throw
        }

        val valParser: Parser[Json] = regex(".".r).flatMap(findParser(_))

        valParser
    }
}