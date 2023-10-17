package chapter7

import java.util.concurrent.{ Callable, CountDownLatch, Executors, ExecutorService, Future, TimeUnit }
import java.util.concurrent.atomic.AtomicReference
import scala.Tuple
import scala.annotation.targetName
import util.Utils.*

opaque type Fyoocher[+A] = (A => Unit) => Unit
opaque type Par[+A] = ExecutorService => Fyoocher[A]

object Par:
    def unit[A](a: A): Par[A] = es => cb => cb(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def delay[A](pa: => Par[A]): Par[A] =
        es => pa(es)

    def fork[A](a: => Par[A]): Par[A] =
        es => cb => eval(es)(a(es)(cb))

    def eval(es: ExecutorService)(r: => Unit): Unit =
        es.submit(new Callable[Unit] { override def call = r })

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
        es => cb =>
            var ar: Option[A] = None
            var br: Option[B] = None
            // this implementation is a little too liberal in forking of threads -
            // it forks a new logical thread for the actor and for stack-safety,
            // forks evaluation of the callback `cb`
            val combiner = Actor[Either[A, B]](es) {
                case Left(a) =>
                    if br.isDefined then eval(es)(cb(f(a, br.get)))
                    else ar = Some(a)
                case Right(b) =>
                    if ar.isDefined then eval(es)(cb(f(ar.get, b)))
                    else br = Some(b)
            }
            pa(es)(a => combiner ! Left(a))
            pb(es)(b => combiner ! Right(b))

    def sortPar[A: Ordering](parList: Par[List[A]]): Par[List[A]] =
        parList map { _.sorted }

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
        fork {
            val fbs: List[Par[B]] = as map { asyncF(f) }
            sequence(fbs)
        }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
        fork {
            val fas: List[Par[Option[A]]] = as map { asyncF { aa => if f(aa) then Some(aa) else None } }
            sequence(fas) map { _.flatten }
        }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldLeft(unit(Nil: List[A])) { (acc, pa) => map2(pa, acc) { _ :: _ } }

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
        fMap(n) { choices(_) }
        // es => choices(n.run(es)).run(es)

    def choice[A](pb: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        fMap(pb) { b => if b then t else f }
        //choiceN(pb map { b => if b then 0 else 1 })(List(t, f))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
        fMap(key) { choices(_) }
        // es => choices(key.run(es)).run(es)

    def fMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es =>
        f(pa.run(es))(es)

    def join[A](ppa: Par[Par[A]]): Par[A] =
        fMap(ppa) { identity }

    def fmap_via_join[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
        join(pa map f)

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
        p.run(e) == p2.run(e)

    def map2Sig_fm_unit[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
        es =>
            (for {
                a <- pa
                b <- pb
            } yield f(a, b))(es)

    extension [A](pa: Par[A])
        def map[B](f: A => B): Par[B] =
            map2(pa, unit(())) { (a, _) => f(a) }

        def flatMap[B](f: A => Par[B]): Par[B] =
            fMap(pa)(f)

        def equal(es: ExecutorService)(p2: Par[A]): Boolean =
            Par.equal(es)(pa, p2)
            
        def run(es: ExecutorService): A =
            p("running")
            val ref = new AtomicReference[A]
            val latch = new CountDownLatch(1)
            pa(es) { a =>
                ref.set(a)
                latch.countDown
            }
            latch.await
            p("latch has... latched")
            ref.get

    val es = Executors.newFixedThreadPool(10)

    @main
    def pars(): Unit =
        def longFilter(i: Int): Boolean =
            Thread.sleep(1000)
            i % 2 == 0

        def timeMap(): Unit =
            p("time map")
            val (e1, l1) = time {
                List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0) map { longFilter }
            }
            p(e1, l1)

            val (e2, l2) = time {
                Par.parMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)) { longFilter }  run es
            }
            p(e2, l2)

        def runActorMap2(): List[Double] =
            p("actor map2")
            val pl = Par.parMap(List.range(1, 10000))(math.sqrt(_))
            val (e, l) = time {
                pl.run(Executors.newFixedThreadPool(1))
            }
            p(e, l)
            l

        try
            //val f1 =  es.submit(() => {
            //    Thread.sleep(5_000)
            //    "f1 val"
            //})
            //val f2 = es.submit(() => {
            //    Thread.sleep(7_000)
            //    "f2 val"
            //})

            runActorMap2().take(10)

        finally
          p("shutting down")
          es.shutdown()

