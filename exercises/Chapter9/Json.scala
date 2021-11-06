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

    def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[Json] = {
        import P._

        lazy val spaces: Parser[String] = char(' ').*.slice

        def trim[A](p: => Parser[A]): Parser[A] = p <** spaces

        lazy val keyValuePairParser: Parser[(String, Json)] =
            (stringParser.map(_.get) <** trim(char(':'))) ** valParser

        def csvParser[E, C, J](
          elemParser: => Parser[E],
          pfx: Char, sfx: Char,
          initialColl: => C, toColl: List[E] => C, joiner: (C, E) => C,
          toJson: C => J,
        ): Parser[J] = {
            val prefix = trim(char(pfx))
            val suffix = char(sfx)
            val comma = trim(char(','))

            enclose(prefix, suffix)(delimited(comma)(elemParser)) map (toColl) map (toJson)
        }

        def valParser: Parser[Json] = trim(nullParser | stringParser | boolParser | arrayParser | objParser | numParser)

        def nullParser: Parser[JNull.type] = trim(string("null")).flatMap(_ => succeed(JNull))
        def stringParser: Parser[JString] = trim(regex("\".*\"".r)).map(JString(_))
        def boolParser: Parser[JBool] = trim(("true" | "false")).map(_.toBoolean).map(JBool(_))
        def arrayParser: Parser[JArray] =
            csvParser[Json, Vector[Json], JArray](valParser, '[', ']', Vector.empty, _.toVector, _ :+ _, JArray(_))
        def objParser: Parser[JObject] =
            csvParser[(String, Json), Map[String, Json], JObject](keyValuePairParser, '{', '}', Map.empty, _.toMap, _ + _, JObject(_))
        def numParser: Parser[JNumber] = trim(regex(raw"[^] },]+".r)).map(_.toDouble).map(JNumber(_))

        trim(spaces **> valParser)
    }
}
