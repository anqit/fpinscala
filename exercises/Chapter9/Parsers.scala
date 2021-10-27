package Chapter9

import Chapter8.{ Gen, Prop }

trait Parsers[ParseError, Parser[+_]] { self =>
    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
        ParserOps(f(a))

    def char(c: Char): Parser[Char] =
        string(c.toString) map { _.charAt(0) }

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

    def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)]

    def succeed[A](a: A): Parser[A] =
        string("") map { _ => a}

    // temp
    def filter[A](p: Parser[A])(f: A => Boolean): Parser[A]

    def many[A](p: Parser[A]): Parser[List[A]]

    def any(a: Parser[String]): Parser[Int] =
        map(many(a))(_.length)

    def atLeastOne(a: Parser[String]): Parser[Int] =
        filter(any(a))(_ > 0)

    def andThen(first: Parser[String])(second: Parser[String]): Parser[(Int, Int)] =
        map2(any(first), any(second))((_, _)) // TODO: not exactly, need to shrink input...

    def map[A, B](p: Parser[A])(f: A => B): Parser[B]

    def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C] =
        product(pa, pb) map { case (a, b) => f(a, b) }

    def many1[A](p: Parser[A]): Parser[List[A]] =
        map2(p, many(p))(_ :: _)

    // returns matching portion of input
    def slice[A](p: Parser[A]): Parser[String]

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
        def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

        def map[B](f: A => B): Parser[B] = self.map(p)(f)

        def slice: Parser[String] = self.slice(p)

        def * = self.many(p)
        def many = self.many(p)

        def + = self.many1(p)
        def many1 = self.many1(p)

        def **[B](p2: Parser[B]) = self.product(p, p2)
        def product[B](p2: Parser[B]) = self.product(p, p2)
    }

    val numA: Parser[Int] = char('a').*.map(_.size)

    val numASlice: Parser[Int] = char('a').*.slice.map(_.size)

    val zeroOrMoreAsThenSomeBs: Parser[(Int, Int)] =
         char('a').*.slice.map(_.size) ** char('b').+.slice.map(_.size)

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
        //        given some input s
        //        val sliceA = run(slice(parserA))(s) // <- assuming this succeeds
        //        val rest = s - sliceA // <- remove sliceA from beginning of s
        //      run(parserA ** parserB)(s) = (sliceA, run(slice(parserB))(rest))
    }
}

