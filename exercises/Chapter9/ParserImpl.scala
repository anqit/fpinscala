package Chapter9

import Chapter9.ParserTypes._

import scala.util.matching.Regex

object ParserTypes {
    type Parser[+A] = Location => Result[A]

    trait Result[+A] {
        def mapError(f: ParseError => ParseError): Result[A] = this match {
            case Failure(e, committed) => Failure(f(e), committed)
            case _ => this
        }

        def uncommit: Result[A] = this match {
            case Failure(e, true) => Failure(e, false)
            case _ => this
        }

        def addCommit(committed: Boolean): Result[A] = this match {
            case Failure(e, c) => Failure(e, c || committed)
            case _ => this
        }

        def advanceSuccess(n: Int): Result[A] = this match {
            case Success(a, consumed) => Success(a, consumed + n)
            case r => r
        }
    }
    case class Success[+A](get: A, consumed: Int) extends Result[A]
    case class Failure(get: ParseError, isCommitted: Boolean = true) extends Result[Nothing]
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
                case f : Failure => f
            }
    }

    def attempt[A](p: Parser[A]): Parser[A] = l => p(l).uncommit

    def errorLocation(e: ParseError): Location = e.stack.head._1

    def errorMessage(e: ParseError): String = e.stack.head._2

    def filter[A](p: Parser[A])(f: A => Boolean): Parser[A] = loc => p(loc) match {
        case s @ Success(a, _) if f(a) => s
        case Success(_, consumed) => Failure(ParseError(loc.advanceBy(consumed), "filter failed"), true)
        case f: Failure => f
    }

    def flatMap[A, B](p1: Parser[A])(f: A => Parser[B]): Parser[B] = l => p1(l) match {
        case Success(a, consumed) => f(a)(l.advanceBy(consumed)).addCommit(consumed != 0).advanceSuccess(consumed)
        case f : Failure => f
    }

    def nonStrict[A](p: => Parser[A]): Parser[A] = p

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = l => p1(l) match {
        case Failure(_, false) => p2(l)
        case r => r
    }

    def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(Location(input, 0)) match {
        case Success(a, _) => Right(a)
        case Failure(e, _) => Left(e)
    }

    def label[A](msg: String)(p: Parser[A]): Parser[A] = l =>
      p(l) mapError { _.label(msg) }

    def scope[A](msg: String)(p: Parser[A]): Parser[A] = l =>
      p(l).mapError(_.push(l, msg))
}

object Main extends App {
    import ParsersImpl._

    val jsonParser = JsonParser.jsonParser(ParsersImpl)
    println(jsonParser.run(""))
}
