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

        lazy val spaces: Parser[String] = char(' ').many.slice

        def csvParser[E, C, J](elemParser: => Parser[E], prefix: Char, suffix: Char, initialColl: => C, toColl: List[E] => C, joiner: (C, E) => C, toJ: C => J): Parser[J] = {
            val emptyCsvParser: Parser[J] = char(prefix).**(spaces).**(char(suffix)).map { _ => toJ(initialColl) }

            val nonEmptyCsvParser: Parser[J] =
                char(prefix).**(spaces).**(elemParser.**(char(',')).*.map { _.map { _._1 } }).map { case ((_, _), l) => l }.map { toColl }
                  .**(elemParser).**(char(suffix)).map {
                    case ((init, last), _) => (init, last)
                }.map { joiner.tupled }.map { toJ }

            emptyCsvParser | nonEmptyCsvParser
        }

        lazy val valParser: Parser[Json] = spaces.**(
            nullParser | stringParser | boolParser | arrayParser | objParser | numParser
        ).**(spaces) map {
            case ((_, jval), _) => jval
        }

        lazy val keyValuePairParser: Parser[(String, Json)] =
            spaces.**(stringParser.map(_.get)).**(spaces).**(char(':')).**(valParser).map {
                case ((((_, key), _), _), jval) => (key, jval)
            }

        lazy val nullParser: Parser[JNull.type] = string("null").flatMap(_ => succeed(JNull))
        lazy val stringParser: Parser[JString] = regex("\".*\"".r).map(JString(_))
        lazy val boolParser: Parser[JBool] = ("true" | "false").map(_.toBoolean).map(JBool(_))
        lazy val arrayParser: Parser[JArray] =
            csvParser[Json, Vector[Json], JArray](valParser, '[', ']', Vector.empty, _.toVector, _ :+ _, JArray(_))
        lazy val objParser: Parser[JObject] =
            csvParser[(String, Json), Map[String, Json], JObject](keyValuePairParser, '{', '}', Map.empty, _.toMap, _ + _, JObject(_))
        lazy val numParser: Parser[JNumber] = regex(raw"[^] },]".r).map(_.toDouble).map(JNumber(_))

        valParser
    }
}