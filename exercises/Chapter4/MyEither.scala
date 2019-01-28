trait MyEither[+E, +A] {
    def map[B](f: A => B): MyEither[E, B] = this match {
        case Left(l) => Left(l)
        case Right(r) => Right(f(r))
    }

    def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
        case x @ Left(l) => x
        case Right(r) => f(r)
    }

    def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
        case Left(_) => b
        case Right(_) => this
    }

    def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
        for {
            aa <- this
            bb <- b
        } yield f(aa, bb)
        // flatMap(a => b map ( bb => f(a, bb)))
}

case class Left[+E](value: E) extends MyEither[E, Nothing]
case class Right[+A](value: A) extends MyEither[Nothing, A]

object MyEither {
    def Try[A](a: => A): MyEither[Exception, A] =
        try Right(a)
        catch { case e: Exception => Left(e) }

    def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
        as.foldRight[MyEither[E, List[B]]](Right(Nil))((a, acc) => f(a).map2(acc)(_ :: _))

    def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = 
        traverse(es)(a => a)

    def main(args: Array[String]) = {
        val r = Right(8)
        val r2 = Right(9)
        val f2: Function1[(Int, Int) => Int, MyEither[Exception, Int]] = (r map2[Exception, Int, Int] r2)_
        val result = f2((a,b) => a*b)
        println("result: " + result)
    }
}
