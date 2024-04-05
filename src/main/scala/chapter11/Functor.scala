package chapter11

trait Functor[F[_]]:
    extension [A](fa: F[A])
        def map[B](f: A => B): F[B]

    extension[A, B] (fab: F[(A, B)])
        def distribute: (F[A], F[B]) =
            (fab.map(_(0)), fab.map(_(1)))

    extension[A, B] (e: Either[F[A], F[B]])
        def codistribute: F[Either[A, B]] =
            e match
                case Left(fa) => fa.map(Left(_))
                case Right(fb) => fb.map(Right(_))
                