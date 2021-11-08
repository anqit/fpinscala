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

class JsonParserFactory[Parser[+_]](P: Parsers[Parser]) {
    import Json._
    import P._

    def valParser: Parser[Json] = trim(nullParser | stringParser | boolParser | arrayParser | objParser | numParser)
    def nullParser: Parser[JNull.type] = trim(string("null")).map(_ => JNull) scope "NULL"
    def stringParser: Parser[JString] = trim(char('"') **> regex("[^\"]*".r) <** char('"')).map(JString(_)) scope "json string"
    def boolParser: Parser[JBool] = trim(("true" | "false")).map(_.toBoolean).map(JBool(_)) scope "json bool"
    def arrayParser: Parser[JArray] =
        csvParser[Json, Vector[Json], JArray](valParser, '[', ']', _.toVector, JArray(_)) scope "json array"
    def objParser: Parser[JObject] =
        csvParser[(String, Json), Map[String, Json], JObject](keyValuePairParser, '{', '}', _.toMap, JObject(_)) scope "json obj"
    def numParser: Parser[JNumber] = trim(regex(raw"[^] },]+".r)).map(_.toDouble).map(JNumber(_)) scope "json num"

    def keyValuePairParser: Parser[(String, Json)] =
        ((stringParser.map(_.get) <** trim(char(':'))) ** valParser) scope "keyval pair"

    def csvParser[E, C, J](
      elemParser: Parser[E],
      pfx: Char, sfx: Char,
      toColl: List[E] => C,
      toJson: C => J,
    ): Parser[J] = {
        val prefix = trim(char(pfx))
        val suffix = trim(char(sfx))
        val comma = trim(char(','))

        enclose(prefix, suffix)(delimited(comma)(elemParser)) map (toColl) map (toJson)
    }

    def jsonParser: Parser[Json] = trim(spaces **> valParser) scope "ROOT"
}

object JsonParserFactory {
    def apply[Parser[+_]](P: Parsers[Parser]) = new JsonParserFactory(P)
}
