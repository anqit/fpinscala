package Chapter11

trait Functor[F[_]] {
    def map[A, B](a: F[A])(f: A => B): F[B]

    def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
        (map(fab)(_._1), map(fab)(_._2))

    def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
        e match {
            case Left(a) => map(a)(Left(_))
            case Right(b) => map(b)(Right(_))
        }
}
