package Chapter11

import Chapter7.Par
import Chapter7.Par.Par
import Chapter8.Gen
import Chapter9.{ Parsers, ParsersImpl }
import Chapter9.ParserTypes.Parser
import jdk.nashorn.api.tree.ParserImpl
import Chapter5.Stream
import Chapter6.State

trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
        (map(fab)((_._1)), map(fab)((_._2)))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
        e match {
            case Left(a) => map(a)(Left(_))
            case Right(b) => map(b)(Right(_))
        }
}

trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
        flatMap(fa)(a => unit(f(a)))

    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
        flatMap(fa)(a => map(fb)(b => f(a, b)))

    def sequence[A](fs: List[F[A]]): F[List[A]] =
        fs.foldRight(unit(List[A]()))((fa, fas) => map2(fa, fas)(_ :: _))

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
        as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = n match {
        case p if p > 0 =>
            map2(ma, replicateM(n - 1, ma))(_ :: _)
        case _ => unit(Nil)
    }

    def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
        as.foldRight(unit(List[A]())) { (a, fas) =>
            map2(f(a), fas) { (bool, las) =>
                if (bool) a :: las else las
            }
        }

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
        flatMap(f(a))(g)

    def flatMap_viaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
        compose[Unit, A, B](_ => fa, f)(())

    def join[A](mma: F[F[A]]): F[A] =
        flatMap(mma)(x => x)

    // ex 11.13
    def compose_viaJoinMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a =>
        join(map(f(a))(g))

    def flatMap_viaJoinMap[A, B](m: F[A])(f: A => F[B]): F[B] =
        join(map(m)(f))
}

object MonadImpl {
    val genMonad = new Monad[Gen] {
        override def unit[A](a: => A): Gen[A] = Gen.unit(a)

        override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
            fa flatMap f
    }

    val parMonad = new Monad[Par] {
        override def unit[A](a: => A): Par[A] = Par.unit(a)

        override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
            Par.chooser(fa)(f)
    }

    def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
        override def unit[A](a: => A): P[A] = p.succeed(a)

        override def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] =
            p.flatMap(fa)(f)
    }

    val optionMonad = new Monad[Option] {
        override def unit[A](a: => A): Option[A] = Option(a)

        override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
            fa flatMap f
    }

    val streamMonad = new Monad[Stream] {
        override def unit[A](a: => A): Stream[A] = Stream(a)

        override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
            fa flatMap f
    }

    val listMonad = new Monad[List] {
        override def unit[A](a: => A): List[A] = List(a)

        override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
            fa flatMap f
    }

    def stateMonad[S] = {
        type S1[x] = State[S, x]
        new Monad[S1] {
            override def unit[A](a: => A): State[S, A] = State.unit(a)

            override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
                fa flatMap f
        }
    }

    def main(args: Array[String]): Unit = {
        val digits = List(0,1,2,3,4) // ,5,6,7,8,9)
        val f: Int => List[Boolean] = i => List(1, i, i * i).map(_ % 2 == 0)
        // not terribly helpful for the list monad
        //println(listMonad.filterM(digits)(f1))

        val f2: Int => List[Boolean] = i => List(i % 2 == 0)
        println(listMonad.filterM(digits)(f2))
    }
}

case class Reader[R, A](run: R => A) {
    def apply(r: R): A = run(r)
}

object Reader {
    def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
        override def unit[A](a: => A): Reader[R, A] =
            Reader(_ => a)

        override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader { r =>
            f(fa(r))(r)
        }
    }

    // r => r => a
    def join[R, A](rra: Reader[R, Reader[R, A]]): Reader[R, A] = Reader { r =>
        rra(r)(r)
    }

}