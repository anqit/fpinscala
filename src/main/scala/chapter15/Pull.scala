package chapter15

import chapter10.Monoid
import chapter11.Monad
import chapter13.IO

import java.nio.file.{ Files, Paths }
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.chaining.scalaUtilChainingOps
import scala.util.Using

opaque type Stream[+O] = Pull[O, Unit]

enum Pull[+O, +R]:
    case Result[+R](result: R) extends Pull[Nothing, R]
    case Output[+O](value: O) extends Pull[O, Unit]
    case FlatMap[X, +O, +R](source: Pull[O, X], f: X => Pull[O, R]) extends Pull[O, R]

    import Pull.*

    def flatMap[O2 >: O, R2](f: R => Pull[O2, R2]): Pull[O2, R2] =
        Pull.FlatMap(this, f)
        
    def step: Either[R, (O, Pull[O, R])] = this match
        case Result(r) => Left(r)
        case Output(o) => Right(o, done)
        case FlatMap(source, f) => source match
            case FlatMap(s2, g) =>
                s2.flatMap(x => g(x).flatMap(f)).step
            case other => other.step match
                case Left(r) => f(r).step
                case Right(o, p) => Right(o, p.flatMap(f))
    
    @tailrec
    final def fold[A](init: A)(f: (A, O) => A): (R, A) =
        step match
            case Left(r) => (r, init)
            case Right(o, p) => p.fold(f(init, o))(f)
    
    def toList: List[O] =
        fold(List.newBuilder[O])((os, o) => os += o)._2.result

    def >>[O2 >: O, R2](next: => Pull[O2, R2]): Pull[O2, R2] =
        flatMap(_ => next)
        
    def map[R2](f: R => R2): Pull[O, R2] =
        flatMap(r => Result(f(r)))

    def repeat: Pull[O, Nothing] =
        this >> repeat

    def uncons: Pull[Nothing, Either[R, (O, Pull[O, R])]] =
        Pull.done >> Result(step)

    def take(n: Int): Pull[O, Option[R]] =
        if n <= 0 then Result(None)
        else uncons.flatMap:
            case Left(r) => Result(Some(r))
            case Right((hd, tl)) => Output(hd) >> tl.take(n - 1)

    def drop(n: Int): Pull[O, R] =
        if n <= 0 then this
        else uncons.flatMap:
            case Left(r) => Result(r)
            case Right((_, tl)) => tl.drop(n - 1)

    def takeWhile(p: O => Boolean): Pull[O, Pull[O, R]] =
        uncons.flatMap:
            case Left(r) => Result(Result(r))
            case Right((hd, tl)) =>
                if p(hd) then
                    Output(hd) >> tl.takeWhile(p)
                else
                    Result(Output(hd) >> tl)

    def dropWhile(p: O => Boolean): Pull[Nothing, Pull[O, R]] =
        uncons.flatMap:
            case Left(r) => Result(Result(r))
            case Right(hd, tl) =>
                if p(hd) then
                    tl.dropWhile(p)
                else
                    Result(Output(hd) >> tl)

    def count: Pull[Int, R] =
        def go(total: Int, p: Pull[O, R]): Pull[Int, R] =
            p.uncons.flatMap:
                case Left(r) => Result(r)
                case Right((_, tl)) =>
                    val newTotal = total + 1
                    Output(newTotal) >> go(newTotal, tl)
        Output(0) >> go(0, this)

    def tally[O2 >: O](using m: Monoid[O2]): Pull[O2, R] =
        def go(acc: O2, p: Pull[O2, R]): Pull[O2, R] =
            p.uncons.flatMap:
                case Left(r) => Result(r)
                case Right((o2, tl)) =>
                    val updated = m.combine(acc, o2)
                    Output(updated) >> go(updated, tl)

        Output(m.identity) >> go(m.identity, this)

    def mapAccumulate[S, O2](init: S)(f: (S, O) => (S, O2)): Pull[O2, (S, R)] =
        uncons.flatMap:
            case Left(r) => Result((init, r))
            case Right((hd, tl)) =>
                val (s, out) = f(init, hd)
                Output(out) >> tl.mapAccumulate(s)(f)

    def countViaMapAccumulate: Pull[Int, R] =
        Output(0) >> mapAccumulate(0): (s, _) =>
            (s + 1, s + 1)
        .map(_._2)

    def tallyViaMapAccumulate[O2 >: O](using m: Monoid[O2]): Pull[O2, R] =
        Output(m.identity) >> mapAccumulate(m.identity): (s, o) =>
            val o2 = m.combine(s, o)
            (o2, o2)
        .map(_._2)

    def filter(p: O => Boolean): Pull[O, R] =
        uncons.flatMap:
            case Left(r) => Result(r)
            case Right(hd, tl) =>
                if p(hd) then Output(hd) >> tl.filter(p) else tl.filter(p)
end Pull

object Pull:
    val done: Pull[Nothing, Unit] = Result(())

    def fromList[O](os: List[O]): Pull[O, Unit] =
        os match
            case Nil => done
            case hd :: tl => Output(hd) >> fromList(tl)

    def fromLazyList[O](os: LazyList[O]): Pull[O, Unit] =
        os match
            case LazyList() => done
            case hd #:: tl => Output(hd) >> fromLazyList(tl)

    def unfold[O, R](init: R)(f: R => Either[R, (O, R)]): Pull[O, R] =
        f(init) match
            case Left(r) => Result(r)
            case Right((o, r2)) => Output(o) >> unfold(r2)(f)

    def fromListViaUnfold[O](os: List[O]): Pull[O, Unit] =
        unfold(os):
            case Nil => Left(Nil)
            case h :: t => Right(h, t)
        >> done

    def fromLazyListViaUnfold[O](os: LazyList[O]): Pull[O, Unit] =
        unfold(os):
            case LazyList() => Left(LazyList.empty)
            case h #:: t => Right(h, t)
        >> done

    def continually[A](a: A): Pull[A, Nothing] =
        Output(a).repeat

    def iterate[O](initial: O)(f: O => O): Pull[O, Nothing] =
        Output(initial) >> iterate(f(initial))(f)

    extension [R](self: Pull[Int, R])
        def slidingMean(n: Int): Pull[Double, R] =
            def go(window: Queue[Int], p2: Pull[Int, R]): Pull[Double, R] =
                p2.uncons.flatMap:
                    case Left(r) => Result(r)
                    case Right(i, tl) =>
                        val updatedWindow = if window.size < n then window :+ i else window.tail :+ i
                        val mean = updatedWindow.sum / updatedWindow.size.toDouble
                        Output(mean) >> go(updatedWindow, tl)

            go(Queue.empty, self)

        def slidingMeanViaMapAccumulate(n: Int): Pull[Double, R] =
            self.mapAccumulate(Queue.empty[Int]): (window, i) =>
                val updatedWindow = if window.size < n then window :+ i else window.tail :+ i
                val mean = updatedWindow.sum / updatedWindow.size.toDouble
                (updatedWindow, mean)
            .map(_._2)

    given [O]: Monad[[x] =>> Pull[O, x]] with
        override def unit[A](a: => A): Pull[O, A] = Result(a)
        extension [A](pa: Pull[O, A])
            override def flatMap[B](f: A => Pull[O, B]): Pull[O, B] =
                pa.flatMap(f)

    extension [O](self: Pull[O, Unit])
        def flatMapOutput[O2](f: O => Pull[O2, Unit]): Pull[O2, Unit] =
            self.uncons.flatMap:
                case Left(()) => Result(())
                case Right((hd, tl)) =>
                    f(hd) >> tl.flatMapOutput(f)

    val outputMonad: Monad[[x] =>> Pull[x, Unit]] = new:
        override def unit[A](a: => A): Pull[A, Unit] = Output(a)
        extension [A](pa: Pull[A, Unit])
            override def flatMap[B](f: A => Pull[B, Unit]): Pull[B, Unit] =
                pa.flatMapOutput(f)

end Pull

object Stream:
    def apply[O](os: O*): Stream[O] =
        Pull.fromList(os.toList).toStream

    def fromIterator[O](itr: Iterator[O]): Stream[O] =
        Pull.unfold(itr)(itr =>
            if itr.hasNext then Right((itr.next(), itr))
            else Left(itr)
        ).void.toStream

    def processFile[A](
      file: java.io.File,
      p: Pipe[String, A],
    )(using m: Monoid[A]): IO[A] = IO:
        val source = scala.io.Source.fromFile(file)
        try fromIterator(source.getLines).pipe(p).fold(m.identity)(m.combine)._2
        finally source.close()

    def toCelsius(fahrenheit: Double): Double =
        (5.0 / 9.0) * (fahrenheit - 32.0)

    def convert(inputFile: String, outputFile: String): IO[Unit] = IO:
        val nonEmpty: Pipe[String, String] = ss => ss.filter(s => !s.isBlank)
        val nonComments: Pipe[String, String] = ss => ss.filter(s => s.charAt(0) != '#')
        val toF: Pipe[String, Double] = ss => ss.flatMapOutput(s => Pull.Output(s.toDouble))
        val toC: Pipe[Double, Double] = fs => fs.map(toCelsius)

        val convertFToCPipe: Pipe[String, Double] =
            nonEmpty andThen nonComments andThen toF andThen toC

        Using.Manager: use =>
            val source = use(scala.io.Source.fromFile(inputFile))
            val writer = use(Files.newBufferedWriter(Paths.get(outputFile)))
            
            fromIterator(source.getLines).pipe(convertFToCPipe).fold(()): (_, c) =>
                writer.write(c.toString)
                writer.newLine()


    extension [O](self: Stream[O])
        def toPull: Pull[O, Unit] = self

        def fold[A](init: A)(f: (A, O) => A): A =
            self.fold(init)(f)(1)

        def toList: List[O] =
            self.toList

        def take(n: Int): Stream[O] =
            self.take(n).map(_ => ())

        def ++(that: => Stream[O]): Stream[O] =
            self >> that

    extension [O](self: Pull[O, Unit])
        def toStream: Stream[O] = self

    given Monad[Stream] with
        def unit[A](a: => A): Stream[A] = Pull.Output(a)
        extension [A](sa: Stream[A])
            override def flatMap[B](f: A => Stream[B]): Stream[B] =
                sa.flatMapOutput(f)
end Stream

type Pipe[-I, +O] = Stream[I] => Stream[O]

object Pipe:
    import Stream.given

    def exists[I](p: I => Boolean): Pipe[I, Boolean] =
        s => s.map(p)
