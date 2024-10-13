package chapter14

opaque type ST[S, A] = S => (A, S)

object ST:
    type Empty = [s] =>> ST[s, scala.Unit]

    extension [S, A](self: ST[S, A])
        def map[B](f: A => B): ST[S, B] =
            s =>
                val (a, s1) = self(s)
                (f(a), s1)

        def flatMap[B](f: A => ST[S, B]): ST[S, B] =
            s =>
                val (a, s1) = self(s)
                f(a)(s1)
    
    def apply[S, A](a: => A): ST[S, A] =
        lazy val memo = a
        s => (memo, s)
        
    def unit[S]: ST.Empty[S] = ST[S, Unit](())
    
    def run[A](st: [s] => () => ST[s, A]): A =
        val su: ST[Unit, A] = st[Unit]()
        su(())(0)
        
    def lift[S, A](f: S => (A, S)): ST[S, A] = f
