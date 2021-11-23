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
    def unit[A](a: A): F[A]
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
}

object Monad {
    val genMonad = new Monad[Gen] {
        override def unit[A](a: A): Gen[A] = Gen.unit(a)

        override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
            fa flatMap f
    }

    val parMonad = new Monad[Par] {
        override def unit[A](a: A): Par[A] = Par.unit(a)

        override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] =
            Par.chooser(fa)(f)
    }

    def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
        override def unit[A](a: A): P[A] = p.succeed(a)

        override def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] =
            p.flatMap(fa)(f)
    }

    val optionMonad = new Monad[Option] {
        override def unit[A](a: A): Option[A] = Option(a)

        override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
            fa flatMap f
    }

    val streamMonad = new Monad[Stream] {
        override def unit[A](a: A): Stream[A] = Stream(a)

        override def flatMap[A, B](fa: Stream[A])(f: A => Stream[B]): Stream[B] =
            fa flatMap f
    }

    val listMonad = new Monad[List] {
        override def unit[A](a: A): List[A] = List(a)

        override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
            fa flatMap f
    }

    def stateMonad[S] = {
        type S1[x] = State[S, x]
        new Monad[S1] {
            override def unit[A](a: A): State[S, A] = State.unit(a)

            override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
                fa flatMap f
        }
    }
}
