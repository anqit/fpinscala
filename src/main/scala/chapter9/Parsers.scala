package chapter9

import chapter8.*

import scala.annotation.targetName
import scala.compiletime.ops.string.+
import scala.util.matching.Regex

trait Parsers[Parser[+_]]:
  def char(c: Char): Parser[c.type] = string(c.toString) map { _.charAt(0).asInstanceOf[c.type] }
  def string(s: String): Parser[s.type]
  def or(s1: String, s2: String): Parser[s1.type | s2.type]
  def and(s1: String, s2: String): Parser[s1.type + s2.type]
  def maybeSome(c: Char): Parser[List[Char]]
  def oneOrMore(c: Char): Parser[List[Char]]
  def countConsecutive(c: Char): Parser[Int]
  def countConsecutiveAtLeastOne(c: Char): Parser[Int]
  def consume[A](s: String): Parser[A]

  def regex(r: Regex): Parser[String]

  def unit[A](a: A): Parser[A] =
    string("") map { _ => a }

  def lazyUnit[A](a: => A): Parser[A]

  val pc: Parser[Char] = char('a')
  val pand: Parser["Hello, World!"] = and("Hello,", " World!")
  val ex9_6: Parser[List[Char]] =
    regex("\\d+".r).map { _.toInt }.flatMap { n => unit('a').listOfN(n) }

  def errorLocation(e: ParseError): Location

  def errorMessage(e: ParseError): String

  extension [A](p: Parser[A])
    def label(s: String): Parser[A]

    def scope(s: String): Parser[A]

    def map[B](f: A => B): Parser[B] =
      flatMap { a => unit(f(a)) }

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      for
        a <- p
        b <- p2
      yield f(a, b)

    def map2_prod[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      **(p2) map { f.tupled }

    def listOfN(n: Int): Parser[List[A]] =
      if n > 0 then map2(listOfN(n - 1)) { _ :: _ } else unit(Nil)

    infix def union(p2: => Parser[A]): Parser[A]

    @targetName("or")
    def |(p2: => Parser[A]): Parser[A] = union(p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      map2(p2) { (a, b) => (a, b) }

    @targetName("then")
    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def maybe(): Parser[Option[A]]

    @targetName("maybeOne")
    def ? : Parser[Option[A]] = maybe()

    def slice(): Parser[String]

    def many(): Parser[List[A]] =
      map2(many()) { _ :: _ } | unit(Nil)

    @targetName("star")
    def * : Parser[List[A]] = many()

    def many1(): Parser[List[A]] =
      map2(many()) { _ :: _ }

    @targetName("oneOrMore")
    def + : Parser[List[A]] = many1()

    def <*[B](p2: Parser[B]): Parser[A] =
      map2(p2.slice()) { (a, _) => a }

    def *>[B](p2: Parser[B]): Parser[B] =
      slice().map2(p2) { (_, b) => b }

    def attempt: Parser[A]

    def run(input: String): Either[ParseError, A]

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(gen: Gen[String]): Prop =
      Prop.forAll(gen) { s => p1.run(s) == p2.run(s) }

    def mapLaw[A](p: Parser[A])(gen: Gen[String]): Prop =
      equal(p, p map identity)(gen)

    def unitLaw[A](c: Char)(gen: Gen[String]): Prop =
      Prop.forAll(gen) { s => unit(c).run(s) == Right(c) }

    def productLaw(gen1: Gen[String], gen2: Gen[String]): Prop =
      Prop.forAll(Gen.map2(gen1, gen2) {  (a, b) => (a, b) }) { (a, b) =>
        (unit(a) ** unit(b)).run(a + b) == Right(a, b)
      }

case class ParseError(stack: List[(Location, String)]):
  def push(l: Location, msg: String): ParseError =
    copy((l, msg) :: stack)

case class Location(input: String, offset: Int = 0):
  private lazy val slice = input.slice(0, offset + 1)

  lazy val line: Int = slice.count(_ == '\n') + 1
  lazy val col: Int = slice.lastIndexOf('\n') match
    case -1 => offset + 1
    case lineStart => offset - lineStart

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))


