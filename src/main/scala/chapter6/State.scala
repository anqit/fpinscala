package chapter6

opaque type State[S, +A] = S => (A, S)

object State:
    extension[S, A] (sa: State[S, A])
        def apply(s: S): (A, S) = sa(s)

        def map[B](f: A => B): State[S, B] =
            s =>
                val (a, next) = sa(s)
                (f(a), next)

        def flatMap[B](f: A => State[S, B]) : State[S, B] =
            s =>
                val (a, next) = sa(s)
                f(a)(next)

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] =
        for
            a <- sa
            b <- sb
        yield f(a, b)

    def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] =
        ss.foldRight(unit(Nil: List[A])) { map2(_, _) { _ :: _ } }

    def get[S]: State[S, S] = s => (s, s)

    def set[S](s: S): State[S, Unit] = _ => ((), s)

    def modify[S](f: S => S): State[S, Unit] =
        // apply[S, Unit] { s => ((), f(s)) }
        for
            s <- get[S]
            _ <- set[S](f(s))
        yield ()

    def apply[S, A](f: S => (A, S)): State[S, A] = f
