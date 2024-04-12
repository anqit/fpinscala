package chapter9

import scala.annotation.targetName
import scala.compiletime.ops.string.+
import scala.util.matching.Regex

enum Result[+A]:
  case Success(get: A, charsConsumed: Int)
  case Failure(get: ParseError) extends Result[Nothing]

  def mapError(f: ParseError => ParseError): Result[A] = this match
    case Failure(e) => Failure(f(e))
    case _ => this


object ParsersImpl extends Parsers[ParsersImpl.Parser]:
  opaque type Parser[+A] = Location => Result[A]
  import chapter9.Result.*

  override def string(s: String): Parser[s.type] =
    {
      case Location(i, o) if i.substring(o, o + s.length) == s =>
        Success[s.type](s, s.length)
      case l => Failure(l.toError(s"expected to see $s starting at ${l.offset}"))
    }

  override def or(s1: String, s2: String): Parser[s1.type | s2.type] = ???

  override def and(s1: String, s2: String): Parser[s1.type + s2.type] = ???

  override def maybeSome(c: Char): Parser[List[Char]] = ???

  override def oneOrMore(c: Char): Parser[List[Char]] = ???

  override def countConsecutive(c: Char): Parser[Int] = ???

  override def countConsecutiveAtLeastOne(c: Char): Parser[Int] = ???

  override def consume[A](s: String): Parser[A] = ???

  override def regex(r: Regex): Parser[String] =
    {
        case l @ Location(input, offset) =>
          val s = input.substring(offset)
          r.findPrefixOf(s) match
            case Some(m) => Success(m, m.length)
            case _ => Failure(l.toError(s"expected regex $r to match starting at $offset"))
    }

  override def unit[A](a: A): Parser[A] =
    _ => Success(a, 0)

  override def lazyUnit[A](a: => A): Parser[A] = ???

  override def errorLocation(e: ParseError): Location = ???

  override def errorMessage(e: ParseError): String = ???

  extension [A] (p: Parser[A])
    override def label(s: String): Parser[A] = ???

    override def scope(s: String): Parser[A] =
      l => p(l).mapError(_.push(l, s))

    override def flatMap[B](f: A => Parser[B]): Parser[B] = ???

    override infix def union(p2: => Parser[A]): Parser[A] = ???

    override def maybe(): Parser[Option[A]] = ???

    override def slice(): Parser[String] = {
      case l @ Location(input, offset) =>
        p(l) match
          case Success(_, consumed) =>
            Success(input.substring(offset, offset + consumed), consumed)
          case f @ Failure(_) => f
    }

    override def attempt: Parser[A] = ???

    override def run(input: String): Either[ParseError, A] =
      p(Location(input)) match
        case Success(a, _) => Right(a)
        case Failure(get) => Left(get)

end ParsersImpl
