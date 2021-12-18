package Chapter12

import Chapter11.Functor

trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
        map2(fa, unit(())) { (a, _) => f(a) }

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
        as.foldRight(unit(List[B]())) { (a, bs)  => map2(f(a), bs) { _ :: _ } }

    def sequence[A](as: List[F[A]]): F[List[A]] =
        traverse(as) { fa => fa }

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
        sequence(List.fill(n)(fa))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
        map2(fa, fb) { (_, _) }

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
        map2(fab, fa)(_(_))

    def map_viaApply[A, B](fa: F[A])(f: A => B): F[B] =
        apply(unit(f))(fa)

    def map2_viaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
        apply(apply(unit[A => B => C](a => f(a, _)))(fa))(fb)

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D) =
        apply(
            apply(
                apply(unit(f.curried))(fa)
            )(fb)
        )(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E) =
        apply(
            apply(
                apply(
                    apply(unit(f.curried))(fa)
                )(fb)
            )(fc)
        )(fd)
}
