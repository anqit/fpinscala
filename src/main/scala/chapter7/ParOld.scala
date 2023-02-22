package chapter7

import java.util.concurrent.*
import scala.Tuple
import scala.annotation.targetName

opaque type ParOld[A] = ExecutorService => Future[A]

object ParOld:
    def unit[A](a: A): ParOld[A] = _ => UnitFuture(a)

    def fork[A](a: => ParOld[A]): ParOld[A] =
        es => es.submit[A]( a(es).get _ )

    def lazyUnit[A](a: => A): ParOld[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => ParOld[B] = a => lazyUnit(f(a))

    def delay[A](pa: => ParOld[A]): ParOld[A] =
        es => pa(es)

    def map2[A, B, C](pa: ParOld[A], pb: ParOld[B])(f: (A, B) => C): ParOld[C] =
        es =>
            val fa = pa(es)
            val fb = pb(es)
            MultiFuture[(A, B)]((fa, fb))  map  { f.tupled }

    def sortPar[A: Ordering](parList: ParOld[List[A]]): ParOld[List[A]] =
        parList map { _.sorted }

    def parMap[A, B](as: List[A])(f: A => B): ParOld[List[B]] =
        fork {
            val fbs: List[ParOld[B]] = as map { asyncF(f) }
            sequence(fbs)
        }

    def parFilter[A](as: List[A])(f: A => Boolean): ParOld[List[A]] =
        fork {
            val fas: List[ParOld[Option[A]]] = as map { asyncF { aa => if f(aa) then Some(aa) else None } }
            sequence(fas) map { _.flatten }
        }

    def sequence[A](ps: List[ParOld[A]]): ParOld[List[A]] =
        ps.foldLeft(unit(Nil: List[A])) { (acc, pa) => map2(pa, acc) { _ :: _ } }

    def equal[A](e: ExecutorService)(p: ParOld[A], p2: ParOld[A]): Boolean =
        p(e).get == p2(e).get

    extension [A](p: ParOld[A])
        def map[B](f: A => B): ParOld[B] =
            map2(p, unit(())) { (a, _) => f(a) }
            
        def run(executorService: ExecutorService): A = p(executorService).get()

case class UnitFuture[A](override val get: A) extends Future[A]:
    override def isDone = true
    override def get(timeout: Long, units: TimeUnit): A = get
    override def isCancelled = false
    override def cancel(evenIfRunning: Boolean): Boolean = false

case class MultiFuture[T <: Tuple](futures: Tuple.Map[T, Future]) extends Future[T]:
    import MultiFuture.*

    override def isDone = futures.toList.map(f => f.asInstanceOf[Future[_]].isDone).reduce(_ && _)
    override def get: T = getAll[T](futures)
    override def get(timeout: Long, units: TimeUnit): T = getAll[T](futures, timeout, units)
    override def isCancelled = futures.toList.map(f => f.asInstanceOf[Future[_]].isCancelled).reduce(_ || _)
    override def cancel(evenIfRunning: Boolean): Boolean = futures.toList.map(f => f.asInstanceOf[Future[_]].cancel(evenIfRunning)).reduce(_ && _)

object MultiFuture:
    def getAll[T <: Tuple](futures: Tuple.Map[T, Future]): T = futures match
        case _: EmptyTuple => EmptyTuple.asInstanceOf[T]
        case fs: Tuple.Map[fh *: ft, Future] =>
            val vh = fs.head.asInstanceOf[Future[fh]].get()
            (vh *: getAll(fs.tail)).asInstanceOf[T]

    def getAll[T <: Tuple](futures: Tuple.Map[T, Future], timeout: Long, units: TimeUnit): T = futures match
        case _: EmptyTuple => EmptyTuple.asInstanceOf[T]
        case fs: Tuple.Map[fh *: ft, Future] =>
            val start = System.nanoTime()
            val vh = fs.head.asInstanceOf[Future[fh]].get(timeout, units)
            val elapsed = System.nanoTime() - start
            val remaining = TimeUnit.NANOSECONDS.convert(timeout, units) - elapsed

            (vh *: getAll(fs.tail, remaining, TimeUnit.NANOSECONDS)).asInstanceOf[T]

    extension [T <: Tuple](mf: MultiFuture[T])
        def map[B](f: T => B): Future[B] =
            Futures.map(mf)(f)

object Futures:
    def map[A, B](fa: Future[A])(f: A => B): Future[B] = new Future[B]:
        override def isDone = fa.isDone
        override def get: B = f(fa.get)
        override def get(timeout: Long, units: TimeUnit): B = f(fa.get(timeout, units))
        override def isCancelled = fa.isCancelled
        override def cancel(evenIfRunning: Boolean): Boolean = fa.cancel(evenIfRunning)

object FT:
    def totuple[A](as: List[A]): Tuple = as match
        case Nil => EmptyTuple
        case h :: t => h *: totuple(t)

    val es = Executors.newFixedThreadPool(10)

    @main
    def ftest(): Unit =
        try
            val f1 =  es.submit(() => {
                Thread.sleep(5_000)
                "f1 val"
            })
            val f2 = es.submit(() => {
                Thread.sleep(7_000)
                "f2 val"
            })

            //println(MultiFuture.getAll[(String, String)]((f1, f2), 7, TimeUnit.SECONDS))

            def longFilter(i: Int): Boolean =
                Thread.sleep(1000)
                i % 2 == 0

            //var now = System.currentTimeMillis()
            //println(now)
            //println(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0) filter { longFilter })
            //println(System.currentTimeMillis() - now)
            //
            //now = System.currentTimeMillis()
            //println(now)
            //println(Par.parFilter(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))(longFilter).run(es))
            //println(System.currentTimeMillis() - now)

            var now = System.currentTimeMillis()
            println(now)
            println(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0) map { longFilter })
            println(System.currentTimeMillis() - now)

            now = System.currentTimeMillis()
            println(now)
            println(ParOld.parMap(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0))(longFilter).run(es))
            println(System.currentTimeMillis() - now)
        finally
          es.shutdown()

