package Chapter11

import Chapter6.State

case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Id {
    def idMonad[A] = new Monad[Id] {
        override def unit[A](a: => A): Id[A] = Id(a)

        override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa flatMap f
    }

    def main(args: Array[String]): Unit = {
        println(
            Id("Hello, ") flatMap { a =>
                Id("monad!") flatMap { b =>
                    Id(a + b)
                }
            }
        )
    }
}


object StateMonad {
    import State._
    def StateMonad[S] = new Monad[({type SS[A] = State[S, A]})#SS] {
        override def unit[A](a: => A): State[S, A] = State.unit(a)

        override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa flatMap f
    }
    val F = StateMonad[Int]

    def zipWithIndex[A](as: List[A]): List[(Int, A)] =
        as.foldLeft(F.unit(List[(Int, A)]())) { (state, a) =>
            for {
                xs <- state
                n <- get
                xxs <- set(n + 1, (n, a) :: xs)
            } yield xxs
        }.run(0)._1.reverse

    def zipWithIndex_book[A](as: List[A]): List[(Int, A)] =
        as.foldLeft(F.unit(List[(Int, A)]())) { (state, a) =>
            for {
                xs <- state
                n <- get
                _ <- set(n + 1)
            } yield (n, a) :: xs
        }.run(0)._1.reverse

    def zipWithIndex_modify[A](as: List[A]): List[(Int, A)] =
        as.foldLeft(F.unit(List[(Int, A)]())) { (state, a) =>
            for {
                xs <- state
                xxs <- State((i: Int) => ((i, a) :: xs, i + 1))
            } yield xxs
        }.run(0)._1.reverse

    def zipWithIndex_transform[A](as: List[A]): List[(Int, A)] =
        as.foldLeft(F.unit(List[(Int, A)]())) { (state, a) =>
            state.flatMap { xs =>
                State((s: Int) => (s, s)).flatMap { n =>
                    State((_: Int) => ((), n + 1)).map { _ =>
                        (n, a) :: xs
                    }
                }
            }
        }.run(0)._1.reverse

    def zipWithIndex_expanded[A](as: List[A]): List[(Int, A)] =
        as.foldLeft(F.unit(List[(Int, A)]())) { (state, a) =>
            State { i =>
                val (xs, next) = state.run(i) // (current list, current index)

                State { (i1: Int) =>
                    val (n, next1) = State((s: Int) => (s, s)).run(i1) // get

                    State { (i2: Int) =>
                        val (_, next2) = State((s: Int) => ((), s + 1)).run(i2) // set

                        ((n, a) :: xs, next2)
                    }(next1)
                }(next)
            }
        }(0)._1.reverse

    def zipWithIndex_compressed[A](as: List[A]): List[(Int, A)] =
        as.foldLeft(F.unit(List[(Int, A)]())) { (state, a) =>
            State { i =>
                val (xs, next) = state.run(i) // (current list, current index)

                ((next, a) :: xs, next + 1)
            }
        }(0)._1.reverse

    def main(args: Array[String]): Unit =
        println(zipWithIndex(List('a', 'b', 'c', 'd')))
        println(zipWithIndex_transform(List('a', 'b', 'c', 'd')))
        println(zipWithIndex_book(List('a', 'b', 'c', 'd')))
        println(zipWithIndex_modify(List('a', 'b', 'c', 'd')))
        println(zipWithIndex_expanded(List('a', 'b', 'c', 'd')))
        println(zipWithIndex_compressed(List('a', 'b', 'c', 'd')))
}