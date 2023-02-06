package Chapter9

import Chapter8.{ Gen, Prop }

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A, B](a: A)(implicit f: A => Parser[B]): ParserOps[B] =
        ParserOps(f(a))
    implicit def regex(r: Regex): Parser[String]

    def char(c: Char): Parser[Char] =
        string(c.toString) map { _.charAt(0) }

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
        case x if x > 0 => map2(p, listOfN(n - 1, p))(_ :: _)
        case _ => succeed(Nil)
    }

    def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
        flatMap(p1)(a => p2.map(b => (a, b)))

    def left[A, B](p1: Parser[A], p2: => Parser[B]): Parser[A] =
        map2(p1, p2)((a, _) => a)

    def right[A, B](p1: Parser[A], p2: => Parser[B]): Parser[B] =
        map2(p1, p2)((_, b) => b)

    def succeed[A](a: A): Parser[A]
        //string("") map { _ => a}

    def filter[A](p: Parser[A])(f: A => Boolean): Parser[A]

    def many[A](p: Parser[A]): Parser[List[A]] =
        map2(p, many(p))(_ :: _) or succeed(List())

    def any(a: Parser[String]): Parser[Int] =
        map(many(a))(_.length)

    def atLeastOne(a: Parser[String]): Parser[Int] =
        filter(any(a))(_ > 0)

    def atMostOne[A](a: Parser[A]): Parser[Option[A]] =
        listOfN(1, a) map { _.headOption }

    def andThen(first: Parser[String])(second: Parser[String]): Parser[(Int, Int)] =
        map2(any(first), any(second))((_, _))

    def flatMap[A, B](p1: Parser[A])(f: A => Parser[B]): Parser[B]

    def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
        flatMap(p)(a => succeed(f(a)))

    def map2[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
        product(pa, pb) map f.tupled

    def map2_flatMap[A, B, C](pa: Parser[A], pb: => Parser[B])(f: (A, B) => C): Parser[C] =
        flatMap(pa)(a => map(pb)(b => f(a, b)))

    def many1[A](p: Parser[A]): Parser[List[A]] =
        map2(p, many(p))(_ :: _)

    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

    // returns matching portion of input
    def slice[A](p: Parser[A]): Parser[String]

    def contextSensitiveCount[A](p: Parser[A]): Parser[List[A]] =
        // flatMap("\\d+".r.map(_.toInt))(listOfN(_, p))
        for {
            digits <- raw"\d+".r
            num = digits.toInt
            l <- listOfN(num, p)
        } yield l

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def nonStrict[A](p: => Parser[A]): Parser[A]

    def label[A](msg: String)(p: Parser[A]): Parser[A]

    def scope[A](msg: String)(p: Parser[A]): Parser[A]

    def attempt[A](p: Parser[A]): Parser[A]

    def errorLocation(e: ParseError): Location

    def errorMessage(e: ParseError): String

    def enclose[A](open: Parser[_], close: Parser[_])(p: => Parser[A]): Parser[A] =
        open **> p <** close

    def delimited[A](delimiter: Parser[_])(p: Parser[A]): Parser[List[A]] =
        p.mapWith(many(delimiter **> p))(_ :: _) or succeed(List())

    case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

        def flatMap[B](f: A => Parser[B]) = self.flatMap(p)(f)
        def map[B](f: A => B): Parser[B] = self.map(p)(f)

        def slice: Parser[String] = self.slice(p)

        def * = self.many(p)
        def many = self.many(p)

        def + = self.many1(p)
        def many1 = self.many1(p)

        def ? = self.atMostOne(p)
        def atMostOne = self.atMostOne(p)

        def **[B](p2: => Parser[B]) = self.product(p, p2)
        def product[B](p2: => Parser[B]) = self.product(p, p2)

        def mapWith[B, C](p2: => Parser[B])(f: (A, B) => C) = self.map2(p, p2)(f)

        def <**(p2: => Parser[_]): Parser[A] = self.left(p, p2)
        def **>[B](p2: => Parser[B]): Parser[B] = self.right(p, p2)

        def label(l: String): Parser[A] = self.label(l)(p)

        def scope(s: String): Parser[A] = self.scope(s)(p)

        def run(input: String): Either[ParseError, A] = self.run(p)(input)
    }

    val numA: Parser[Int] = char('a').*.map(_.size)

    val numASlice: Parser[Int] = char('a').*.slice.map(_.size)

    val zeroOrMoreAsThenSomeBs: Parser[(Int, Int)] =
         char('a').*.slice.map(_.size) ** char('b').+.slice.map(_.size)

    def spaces: Parser[String] = "\\s*".r scope "whitespace"

    def trim[A](p: Parser[A]): Parser[A] = (attempt(p) <** spaces) scope "trim"

    object Laws {
        def equal[A](p1: Parser[A], p2: Parser[A], tag: String = "parser equal law")(in: Gen[String]): Prop =
            Gen.forAll(in, tag)(s => run(p1)(s) == run(p2)(s))

        def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
            equal(p, p.map(a => a), "parser map law")(in)

        def succeedLaw[A](a: A)(in: Gen[String]): Prop =
            Gen.forAll(in, "succeed law")(s => run(succeed(a))(s) == Right(a))

        // product laws
        // 1. if first failed, return that error
        // 2. run(succeed(a) ** succeed(b))(_) = (a, b)
        // 3. run(string(s1) ** string(s2))(s1 + s2 + ...) = (s1, s2) ==> need two gens?
        // 4.
        //      given some input s
        //      val sliceA = run(slice(parserA))(s) // <- assuming this succeeds
        //      val rest = s - sliceA // <- remove sliceA from beginning of s
        //      run(parserA ** parserB)(s) = (sliceA, run(slice(parserB))(rest))
    }
}

case class ParseError private (stack: List[(Location, String)] = List()) {
    def this(l: Location, s: String) = this(List((l, s)))

    def push(loc: Location, msg: String): ParseError =
        copy(stack = (loc, msg) :: stack)

    def label[A](s: String): ParseError =
        ParseError(latestlLoc.map((_, s)).toList)

    def latestlLoc: Option[Location] =
        latest map (_._1)

    def latest: Option[(Location, String)] =
        stack.lastOption
}

object ParseError {
    def apply(l: Location, s: String) = new ParseError().push(l, s)
}

case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
        case -1 => offset + 1
        case linestart => offset - linestart
    }

    def advanceBy(n: Int): Location = copy(offset = offset + n)
}
