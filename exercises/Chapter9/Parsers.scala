package Chapter9

trait Parsers[ParseError, Parser[+_]] { self =>
    def char(c: Char): Parser[Char]

    def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

    def run[A](p: Parser[A])(inprt: String): Either[ParseError, A]


    implicit def string(s: String): Parser[String]
    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
        ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
        def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    }
}
