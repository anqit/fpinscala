package Chapter7

import scala.concurrent.duration.{FiniteDuration, NANOSECONDS, TimeUnit}

//import java.util.concurrent.{ExecutorService, Future}


object Par {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = _.submit(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def fork[A](p: => Par[A]): Par[A] = es => es.submit(p(es).get)

    def delay[A](p: => Par[A]): Par[A] = es => p(es)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
            val af = a(es)
            val bf = b(es)
            (af mapWith bf)(f)
        }

    def map[A, B](a: Par[A])(f: A => B): Par[B] =
        map2(a, unit(()))((a, _) => f(a))

    def run[A](es: ExecutorService)(p: Par[A]): Future[A] = p(es)

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequence[A](pas: List[Par[A]]): Par[List[A]] = {
        pas.foldRight[Par[List[A]]](unit(List()))((pa, as) => map2(pa, as)(_ :: _))
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        val pars: List[Par[List[A]]] = as.map(asyncF(a => if(f(a)) List(a) else List()))

        map(sequence(pars))(_.flatten)
    }

    def parReduce[A, B](as: IndexedSeq[A])(b: B)(acc: (A, B) => B, comb: (B, B) => B): Par[B] = {
        if (as.length == 0) unit(b)
        else if (as.length == 1) asyncF(acc(_, b))(as(0))
        else {
            val (l, r) = as.splitAt(as.length / 2)
            map2(fork(parReduce(l)(b)(acc, comb)), fork(parReduce(r)(b)(acc, comb)))(comb)
        }
    }

    def sum(ints: IndexedSeq[Int]): Par[Int] = parReduce(ints)(0)(_ + _, _ + _)
    def max(ints: IndexedSeq[Int]): Par[Int] = parReduce(ints)(Int.MinValue)((a, b) => if (a > b) a else b, (b1, b2) => if (b1 > b2) b1 else b2)

    type Paragraph = List[String]
    def countWords(paras: IndexedSeq[Paragraph]): Par[Int] = {
//        List[Par[Int]] parParas = paras.map(asyncF(_.length))
        parReduce[Paragraph, Int](paras)(0)(_.length + _, _ + _)
    }

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
        val ab = map2(a, b)((a, b) => f(a, b, _, _))
        val abc = map2(ab, c)((fab, c) => fab(c, _))
        map2(abc, d)(_(_))
    }
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

object Main {
    def main(args: Array[String]): Unit = {
        val es = new ExecutorService {
            override def submit[A](a: => A): Future[A] = UnitFuture(a)
        }
        val ints = IndexedSeq(3, 6, 9, 100, -4)

        val max = Par.max(ints)(es).get
        val sum = Par.sum(ints)(es).get
        println(max)
        println(sum)
    }
}