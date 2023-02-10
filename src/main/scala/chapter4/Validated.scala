package chapter4

enum Validated[+E, +A]:
    case Valid(get: A)
    case Invalid(error: E)

    def toEither: Either[E, A] =
        this match
            case Valid(a) => Either.Right(a)
            case Invalid(es) => Either.Left(es)

    def map[B](f: A => B): Validated[E, B] =
        this match
            case Valid(a) => Valid(f(a))
            case Invalid(es) => Invalid(es)

    def map2[EE >: E, B, C](
      b: Validated[EE, B])(
      f: (A, B) => C)(
      combineErrors: (EE, EE) => EE
    ): Validated[EE, C] =
        (this, b) match
            case (Valid(aa), Valid(bb)) => Valid(f(aa, bb))
            case (Invalid(e), Valid(_)) => Invalid(e)
            case (Valid(_), Invalid(e)) => Invalid(e)
            case (Invalid(e1), Invalid(e2)) => Invalid(combineErrors(e1, e2))

object Validated:
    def fromEither[E, A](e: Either[E, A]): Validated[E, A] =
        e match
            case Either.Right(a) => Valid(a)
            case Either.Left(es) => Invalid(es)

    def traverse[E, A, B](
      as: List[A],
      combineErrors: (E, E) => E,
      f: A => Validated[E, B],
    ): Validated[E, List[B]] =
        as.foldRight(Valid(Nil): Validated[E, List[B]]) { (a, acc) =>
            f(a).map2(acc)(_ :: _)(combineErrors) }

    def sequence[E, A](vs: List[Validated[E, A]], combineErrors: (E, E) => E): Validated[E, List[A]] =
        traverse(vs, combineErrors, identity)