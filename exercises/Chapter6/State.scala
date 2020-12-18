package Chapter6

case class State[S, +A] (run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = State(s => {
        val (a, next) = run(s)
        (f(a), next)
    })

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
        val (a, next) = run(s)
        f(a).run(next)
    })

    def mapWith[B, C](sb: State[S, B])(f: (A, B) => C) = State.map2(this, sb)(f)
}

object State {
    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C) =
        sa.flatMap(a => sb.map(f(a, _)))

    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
        sas.foldRight(unit[S, List[A]](List[A]()))((sa, as) => map2(sa, as)(_ :: _))

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def modify[S](f: S => S): State[S, Unit] = for {
            s <- get
            _ <- set(f(s))
        } yield ()

}