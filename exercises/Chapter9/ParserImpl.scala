package Chapter9

import scala.util.matching.Regex

abstract class ParserImpl[+A](val errorMessage: String) {
    def run(input: String): Either[ParseError, A]
}

object Parser extends Parsers[ParserImpl] {
    implicit def string(s: String): ParserImpl[String] = new ParserImpl[String]("strings didn't match") {
        override def run(input: String): Either[ParseError, String] = {
            def go(expected: List[Char], test: List[Char], offset: Int = 0): Either[ParseError, String] = (expected, test) match {
                case (Nil, Nil) => Right(s)
                case (ehead :: etail, thead :: ttail) if ehead == thead =>
                     go(etail, ttail, offset + 1)
                case _ => Left(ParseError(Location(input, offset), errorMessage))
            }

            go(s.toList, input.toList)
        }
    }

    implicit def regex(r: Regex): ParserImpl[String] =  new ParserImpl[String]("regex doesn't match") {
        override def run(input: String): Either[ParseError, String] = input match {
            case r(m) => Right(m)
            case _ => Left(ParseError(Location(input, 0), errorMessage))
        }
    }

    def attempt[A](p: ParserImpl[A]): ParserImpl[A] = ???

    def errorLocation(e: ParseError): Location = e.stack.head._1

    def errorMessage(e: ParseError): String = e.stack.head._2

    def filter[A](p: ParserImpl[A])(f: A => Boolean): ParserImpl[A] = new ParserImpl[A](p.errorMessage) {
        override def run(input: String): Either[ParseError, A] = p.run(input) match {
            case r @ Right(a) => if (f(a)) r else Left(ParseError(Location(input, 0), "parsed value failed predicate"))
            case l => l
        }
    }

    def flatMap[A, B](p1: ParserImpl[A])(f: A => ParserImpl[B]): ParserImpl[B] = new ParserImpl[B](p1.errorMessage) {
        override def run(input: String): Either[ParseError, B] = p1.run(input) match {
            case r @ Right(a) => f(a).run(input) // todo trim input?
            case Left(e) => Left(e)
        }
    }
    def label[A](msg: String)(p: ParserImpl[A]): ParserImpl[A] = ???
    def nonStrict[A](p: => ParserImpl[A]): ParserImpl[A] = ???
    def or[A](p1: ParserImpl[A], p2: => ParserImpl[A]): ParserImpl[A] = ???
    def run[A](p: ParserImpl[A])(input: String): Either[ParseError,A] = ???
    def scope[A](msg: String)(p: ParserImpl[A]): ParserImpl[A] = ???
    def slice[A](p: ParserImpl[A]): ParserImpl[String] = ???
}
