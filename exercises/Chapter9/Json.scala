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

        def trim[A](p: Parser[A]): Parser[A] = p <** spaces

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
            lazy val comma = trim(char(','))

            val emptyCsvParser: Parser[J] = (prefix ** suffix) map { _ => toJson(initialColl) }

            val elemsParser: Parser[C] = (elemParser <** comma).* map { toColl }

            val nonEmptyCsvParser: Parser[J] =
                prefix **> elemsParser ** elemParser <** suffix map { joiner.tupled } map { toJson }

            emptyCsvParser | nonEmptyCsvParser
        }

        lazy val valParser: Parser[Json] = trim(nullParser | stringParser | boolParser | arrayParser | objParser | numParser)

        lazy val nullParser: Parser[JNull.type] = trim(string("null")).flatMap(_ => succeed(JNull))
        lazy val stringParser: Parser[JString] = trim(regex("\".*\"".r)).map(JString(_))
        lazy val boolParser: Parser[JBool] = trim(("true" | "false")).map(_.toBoolean).map(JBool(_))
        lazy val arrayParser: Parser[JArray] =
            csvParser[Json, Vector[Json], JArray](valParser, '[', ']', Vector.empty, _.toVector, _ :+ _, JArray(_))
        lazy val objParser: Parser[JObject] =
            csvParser[(String, Json), Map[String, Json], JObject](keyValuePairParser, '{', '}', Map.empty, _.toMap, _ + _, JObject(_))
        lazy val numParser: Parser[JNumber] = trim(regex(raw"[^] },]+".r)).map(_.toDouble).map(JNumber(_))

        trim(spaces **> valParser)
    }
}
