package chapter12

import chapter11.Functor

trait Applicative[F[_]] extends Functor[F]:
    def unit[A](a: => A): F[A]

    def apply[A, B](fab: F[A => B], fa: F[A]): F[B] =
        fab.map2(fa) { _(_) }

    def sequence[A](fas: List[F[A]]): F[List[A]] =
        fas.foldRight(unit[List[A]](Nil)) { (fa, as) => fa.map2(as) { _ :: _ } }

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
        as.foldRight(unit[List[B]](Nil)) { (a, bs) => f(a).map2(bs) { _ :: _ } }

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
        sequence(List.fill(n)(fa))

    extension [A](fa: F[A])
        def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
            apply[B, C](apply[A, B => C](unit(a => f.curried(a)), fa), fb)

        def map3[B, C, D](fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
            apply(apply(apply(unit(a => f.curried(a)), fa), fb), fc)

        def map4[B, C, D, E](fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
            apply(apply(apply(apply(unit(a => f.curried(a)), fa), fb), fc), fd)

        def map[B](f: A => B): F[B] =
            apply(unit(f), fa)

        def map_viaMap2[B](f: A => B): F[B] =
            fa.map2(unit(())) { (a, _) => f(a) }

        def product[B](fb: F[B]): F[(A, B)] =
            fa.map2(fb) { (_, _) }
            