package chapter11

import chapter12.Applicative
import chapter6.State

import scala.annotation.targetName

trait Monad[F[_]] extends Applicative[F]:
    def unit[A](a: => A): F[A]

    override def sequence[A](fas: List[F[A]]): F[List[A]] =
        fas.foldLeft(unit[List[A]](Nil)) { (as, fa) =>
            fa.map2(as) { _ :: _ }
        } map { _.reverse }

    override def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
        as.foldLeft(unit[List[B]](Nil)) { (fbs, a) =>
            f(a).map2(fbs) { _ :: _ }
        } map { _.reverse }

    override def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
        def go(count: Int, acc: F[List[A]]): F[List[A]] =
            if (count > 0) fa.map2(go(count - 1, acc)) { _ :: _ } else acc

        go(n, unit(Nil))

    def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
        as.foldLeft(unit[List[A]](Nil)) { (fas, a) =>
            f(a).map2(fas) { (b, acc) => if b then a :: acc else acc }
        } map { _.reverse }

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
        a => f(a).flatMap(g)

    @targetName("joyn")
    def joyn[A](ffa: F[F[A]]): F[A] =
        ffa.flatMap(identity)

    def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] =
        compose(_ => fa, f)(())

    extension [A](fa: F[A])
        override def map[B](f: A => B): F[B] =
            flatMap(a => unit(f(a)))

        def flatMap[B](f: A => F[B]): F[B] =
            fa.map(f).join

        override def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
            fa.flatMap(a => fb.map(b => f(a, b)))

        def fmViaJoinMap[B](f: A => F[B]): F[B] =
            fa.map(f).join

    extension [A](ffa: F[F[A]])
        def join: F[A] = joyn(ffa)

end Monad

object Monad:
    given eitherMonad[E]: Monad[[x] =>> Either[E, x]] with
        override def unit[A](a: => A): Either[E, A] =
            Right(a)

        extension[A](ea: Either[E, A])
            override def flatMap[B](f: A => Either[E, B]): Either[E, B] =
                ea flatMap f
                
    given stateMonad[S]: Monad[[x] =>> State[S, x]] with
        override def unit[A](a: => A): State[S, A] = State.unit(a)
        extension[A](sa: State[S, A])
            override def flatMap[B](f: A => State[S, B]): State[S, B] =
                State.flatMap(sa)(f)

case class Id[A](a: A):
    def map[B](f: A => B): Id[B] =
        Id(f(a))

    def flatMap[B](f: A => Id[B]): Id[B] = f(a)

object Id:
    given idMonad: Monad[Id] with
        override def unit[A](a: => A):Id[A] = Id(a)

        extension [A](ida: Id[A])
            override def flatMap[B](f: A => Id[B]): Id[B] = ida.flatMap(f)


