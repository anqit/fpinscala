package Chapter9

trait Parsers[ParseError, Parser[+_]] { self =>
    def char(c: Char): Parser[Char]

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]


    // temp
    def filter[A](p: Parser[A])(f: A => Boolean): Parser[A]

    def any(a: Parser[String]): Parser[Int] =
        map(a)(_.length)

    def atLeastOne(a: Parser[String]): Parser[Int] =
        filter(any(a))(_ > 0)

    def andThen(first: Parser[String])(second: Parser[String]): Parser[(Int, Int)] =
        map2(any(first), any(second))((_, _)) // TODO: not exactly, need to shrink input...


    def map[A, B](p: Parser[A])(f: A => B): Parser[B]
    def map2[A, B, C](pa: Parser[A], pb: Parser[B])(f: (A, B) => C): Parser[C]

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
        ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
        def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    }
}
