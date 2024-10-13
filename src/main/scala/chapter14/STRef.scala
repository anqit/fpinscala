package chapter14

final class STRef[S, A] private (private var cell: A):
    def read: ST[S,A] = ST(cell)
    def write(a: => A): ST[S, Unit] = ST.lift[S, Unit]:
        s =>
            cell = a
            ((), s)

object STRef:
    def apply[S, A](a: A): ST[S, STRef[S, A]] =
        ST(new STRef[S, A](a))

    def trySTRef(): Unit =
        val p = [s] => () =>
            for
                r1 <- STRef[Nothing, Int](1)
                r2 <- STRef[Nothing, Int](1)
                x <- r1.read
                y <- r2.read
                _ <- r1.write(y + 1)
                _ <- r2.write(x + 1)
                a <- r1.read
                b <- r2.read
            yield (a, b)