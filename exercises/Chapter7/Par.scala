package Chapter7

import scala.concurrent.duration.{FiniteDuration, NANOSECONDS, TimeUnit}

//import java.util.concurrent.{ExecutorService, Future}


object Par {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = _.submit(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def fork[A](p: => Par[A]): Par[A] = es => es.submit(p(es).get)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
            val af = a(es)
            val bf = b(es)
            (af mapWith bf)(f)
        }

    def run[A](es: ExecutorService)(p: Par[A]): Future[A] = p(es)

}

abstract class ExecutorService {
    def submit[A](a: => A): Future[A]
}

trait Future[A] { self =>
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(ifRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean

    def map[B](f: A => B): Future[B] = new Future[B] {
            override def get: B = f(self.get)

            override def get(timeout: Long, unit: TimeUnit): B = f(self.get(timeout, unit))

            override def cancel(ifRunning: Boolean): Boolean = self.cancel(ifRunning)

            override def isDone: Boolean = self.isDone

            override def isCancelled: Boolean = self.isCancelled
        }

    def mapWith[B, C](bf: Future[B])(f: (A, B) => C): Future[C] = new Future[C] {
            override def get: C = f(self.get, bf.get)
            override def get(timeout: Long, unit: TimeUnit): C = {
                val duration = FiniteDuration(timeout, unit).toNanos
                val start = System.nanoTime()
                val a = self.get(timeout, unit)
                val remaining = duration - (System.nanoTime() - start)
                val b = bf.get(remaining, NANOSECONDS)
                f(a, b)
            }

        override def cancel(ifRunning: Boolean): Boolean = {
            val sc = self.cancel(ifRunning)
            val bc = bf.cancel(ifRunning)
            sc || bc
        }

        override def isDone: Boolean = self.isDone && bf.isDone

        override def isCancelled: Boolean = self.isCancelled || bf.isCancelled
    }
}

case class UnitFuture[A](get: A) extends Future[A] {
    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(ifRunning: Boolean): Boolean = false

    override def isDone: Boolean = true

    override def isCancelled: Boolean = false
}
