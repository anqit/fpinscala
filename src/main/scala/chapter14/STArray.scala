package chapter14

import scala.reflect.ClassTag

final class STArray[S, A] private (private var value: Array[A]):
    def size: ST[S, Int] = ST(value.size)

    def write(i: Int, a: A): ST[S, Unit] =
        ST.lift[S, Unit]:
            s =>
                value(i) = a
                ((), s)

    def read(i: Int): ST[S, A] = ST(value(i))

    def freeze: ST[S, List[A]] = ST(value.toList)

    def fill(xs: Map[Int, A]): ST[S, Unit] =
        ST.lift[S, Unit]: s =>
            (xs.foreach(write), s)

    def swap(i: Int, j: Int): ST[S, Unit] =
        for
            x <- read(i)
            y <- read(j)
            _ <- write(i, y)
            _ <- write(j, x)
        yield ()

object STArray:
    def apply[S, A: ClassTag](sz: Int, v: A): ST[S, STArray[S, A]] =
        ST(new STArray[S, A](Array.fill(sz)(v)))

    def fromList[S, A : ClassTag](xs: List[A]): ST[S, STArray[S, A]] =
        ST(new STArray[S, A](xs.toArray))

object Immutable:
    import Ordering.Implicits.given

    def quicksort[A: Ordering: ClassTag](xs: List[A]): List[A] =
        def partition[S](a: STArray[S, A], l: Int, r: Int, pivot: Int): ST[S, Int] =
            def loop(l: Int, r: Int, pivotVal: A, j: STRef[S, Int]): ST.Empty[S] =
                (l until r).foldLeft(ST.unit[S]): (s, i) =>
                    for
                        _ <- s
                        v <- a.read(i)
                        _ <- if v < pivotVal then
                            for
                                jj <- j.read
                                _ <- a.swap(i, jj)
                                _ <- j.write(jj + 1)
                            yield ()
                        else ST.unit[S]
                    yield ()

            for
                pivotVal <- a.read(pivot)
                _ <- a.swap(r, pivot)
                j <- STRef(l)
                _ <- loop(l, r, pivotVal, j)
                jj <- j.read
                _ <- a.swap(jj, r)
            yield jj

        def qs[S](a: STArray[S, A], l: Int, r: Int): ST[S, Unit] =
            if l < r then
                for
                    pi <- partition(a, l, r, (r - l) / 2)
                    _ <- qs(a, l, pi - 1)
                    _ <- qs(a, pi + 1, r)
                yield ()
            else ST(())

        ST.run([s] => () =>
            for
                arr <- STArray.fromList[s, A](xs)
                _ <- qs(arr, 0, xs.length - 1)
                sorted <- arr.freeze
            yield sorted
        )