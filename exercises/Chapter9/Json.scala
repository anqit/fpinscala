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

        val valParser: Parser[Json] = regex(".".r).flatMap(findParser(_))

        def findParser(indicator: String): Parser[Json] = indicator match {
            case "\"" => stringParser
            case raw"\d|-".r =>
        }
    }
}