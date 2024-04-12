package chapter9

enum Json:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[Json])
  case JObject(get: Map[String, Json])

object Json:
  def parser[Err, Parser[+_]](P: Parsers[Parser]): Parser[Json] =
    import P.*

    val terminals = List('}', ']', ',')

    val whitespace = regex("\\s*".r)
    val anyString: Parser[String] = char('"') *> regex("[^\"]".r) <* char('"')
    def csl[A](pa: Parser[A]): Parser[List[A]] =
      val pas = (pa <* (whitespace ** char(',') ** whitespace)).*
      pas ** pa.? map {
        case (as, Some(a)) => as :+ a
        case _ => Nil
      }

    def enclosed[O, V, C](open: Parser[O], value: Parser[V], close: Parser[C]): Parser[V] =
      open *> value <* close

    def enclosedC[A](o: Char, pa: Parser[A], c: Char): Parser[A] =
      enclosed(char(o) ** whitespace, pa, whitespace ** char(c))

    val jnull = string("null") map { _ => JNull }
    val jnumber = regex("[^}\\],]".r) map { _.toDouble } map { JNumber.apply }
    val jstring = anyString map { JString.apply }
    val jbool = string("true") | string("false") map { _.toBoolean } map { JBool.apply }

    def kv: Parser[(String, Json)] =
      for
        k <- anyString
        _ <- whitespace ** char(':') ** whitespace
        v <- jval
      yield (k, v)

    def jval: Parser[Json] = jnull | jnumber | jstring | jbool | jarray | jobject
    def jarray: Parser[JArray] = enclosedC('[', csl(jval), ']') map { _.toIndexedSeq } map { JArray.apply }
    def jobject: Parser[JObject] = enclosedC('{', csl(kv), '}') map { ls => Map(ls*) } map { JObject.apply }

    jobject | jarray

//    val jarray = char('[') ** whitespace ** csl(jval) ** whitespace ** char(']')
