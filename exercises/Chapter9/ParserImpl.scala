package Chapter9

import Chapter9.ParserTypes._

import scala.util.matching.Regex

object ParserTypes {
    type Parser[+A] = Location => Result[A]

    trait Result[+A] {
        def mapError(f: ParseError => ParseError): Result[A] = this match {
            case Failure(e) => Failure(f(e))
            case _ => this
        }
    }
    case class Success[+A](get: A, consumed: Int) extends Result[A]
    case class Failure(get: ParseError) extends Result[Nothing]
}

object ParsersImpl extends Parsers[Parser] {
    private def compareString(input: String, pattern: String, offset: Int) = {
        def go(i: List[Char], p: List[Char], eoffset: Int = 0): Result[String] = (i, p) match {
            case (_, Nil) =>
                Success(pattern, pattern.length)
            case (Nil, _) =>
                Failure(ParseError(Location(input, offset + eoffset), "unexpected eof"))
            case (ihead :: itail, phead :: ptail) if ihead == phead =>
                go(itail, ptail, offset + 1)
            case (ihead :: _, phead :: _) =>
                Failure(ParseError(Location(input, offset + eoffset), s"expected $phead, found $ihead"))
        }

        go(input.slice(offset, input.length).toList, pattern.toList)
    }

    override def string(s: String): Parser[String] = scope(s"input did not match $s") {
        case Location(input, offset) =>
            compareString(input, s, offset)
    }


    override def regex(r: Regex): Parser[String] = {
        case Location(input, offset) =>
            input match {
                case r(m) => Success(m, m.length)
                case _ => Failure(ParseError(Location(input, offset), "regex didn't match"))
            }
    }

    override def succeed[A](a: A): Parser[A] = {
        case Location(input, offset) =>
            Success(a, input.length)
    }

    def slice[A](p: Parser[A]): Parser[String] = {
        case l @ Location(input, offset) =>
            p(l) match {
                case Success(_, consumed) => Success(input.slice(offset, offset + consumed), consumed)
                case f @ Failure(_) => f
            }
    }

    def attempt[A](p: Parser[A]): Parser[A] = ???

    def errorLocation(e: ParseError): Location = e.stack.head._1

    def errorMessage(e: ParseError): String = e.stack.head._2

    def filter[A](p: Parser[A])(f: A => Boolean): Parser[A] = ???

    def flatMap[A, B](p1: Parser[A])(f: A => Parser[B]): Parser[B] = ???

    def nonStrict[A](p: => Parser[A]): Parser[A] = ???
    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = ???
    def run[A](p: Parser[A])(input: String): Either[ParseError,A] = ???

    def label[A](msg: String)(p: Parser[A]): Parser[A] = l =>
      p(l) mapError { _.label(msg) }

    def scope[A](msg: String)(p: Parser[A]): Parser[A] = l =>
      p(l).mapError(_.push(l, msg))
}
