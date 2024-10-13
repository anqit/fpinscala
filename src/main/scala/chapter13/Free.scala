package chapter13

import chapter11.Monad
import chapter7.Par

import scala.io.StdIn.readLine
import scala.util.Try

enum Free[F[_], A]:
  case Return(a: A)
  case Suspend(s: F[A])
  case FlatMap[X[_], Y, B](s: Free[X, Y], f: Y => Free[X, B]) extends Free[X, B]

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Return(f(a)))

  def translate[G[_] : Monad](fToG: [x] => F[x] => G[x]): Free[G, A] =
    Suspend(runFree(fToG))

  def runFree[G[_]](t: [x] => F[x] => G[x])(using G: Monad[G]): G[A] =
    step match
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => t(r).flatMap(a => f(a).runFree(t))
      case FlatMap(_, _) =>
        sys.error("Impossible, `step` eliminates these cases")

  @annotation.tailrec
  final def step: Free[F, A] = this match
    case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(g)).step
    case FlatMap(Return(x), f) => f(x).step
    case _ => this

  def run(using F: Monad[F]): F[A] = step match
    case Return(a) => F.unit(a)
    case Suspend(s) => s
    case FlatMap(x, f) => x match
      case Suspend(s) => s.flatMap(a => f(a).run)
      case _ => sys.error("Impossible, since `step` eliminates these cases")

object Free:
  given freeMonad[F[_]]: Monad[[x] =>> Free[F, x]] with
    override def unit[A](a: => A): Free[F, A] = Return(a)

    extension [A](fa: Free[F, A])
      override def flatMap[B](f: A => Free[F, B]): Free[F, B] =
        fa.flatMap(f)

  extension [A](fa: Free[Function0, A])
    @annotation.tailrec
    def runTrampoline: A = fa match
      case Return(a) => a
      case Suspend(s) => s()
      case FlatMap(x, f) => x match
        case Return(a) => f(a).runTrampoline
        case Suspend(y) => f(y()).runTrampoline
        case FlatMap(y, g) =>
          y.flatMap(a => g(a).flatMap(f)).runTrampoline


enum Async[A]:
  case Return(a: A)
  case Suspend(resume: Par[A])
  case FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]

  def flatMap[B](f: A => Async[B]): Async[B] =
    FlatMap(this, f)

  def map[B](f: A => B): Async[B] =
      flatMap(a => Return(f(a)))

  @annotation.tailrec final def step: Async[A] = this match
    case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(g)).step
    case FlatMap(Return(x), f) => f(x).step
    case _ => this

  def run: Par[A] = step match
    case Return(a) => Par.unit(a)
    case Suspend(r) => r
    case FlatMap(x, f) => x match
      case Suspend(r) => r.flatMap(a => f(a).run)
      case _ => sys.error("Impossible, since `step` eliminates these cases")

enum Console[A]:
  case ReadLine extends Console[Option[String]]
  case PrintLine(line: String) extends Console[Unit]

  def toPar: Par[A] = this match
    case ReadLine => Par.lazyUnit(Try(readLine()).toOption)
    case PrintLine(line) => Par.lazyUnit(println(line))

  def toThunk: () => A = this match
    case ReadLine => () => Try(readLine()).toOption
    case PrintLine(line) => () => println(line)

  def toReader: ConsoleReader[A] = ???

object Console:
  import Free.*

  given function0Monad: Monad[Function0] with
    def unit[A](a: => A) = () => a
    extension [A](fa: () => A)
      override def flatMap[B](f: A => () => B): () => B =
        f(fa())

  given parMonad: Monad[Par] with
    def unit[A](a: => A) = Par.unit(a)
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMap(fa)(f)

  def readLn: Free[Console, Option[String]] =
    Suspend(ReadLine)

  def printLn(line: String): Free[Console, Unit] =
    Suspend(PrintLine(line))

  extension[A] (fa: Free[Console, A])
    def toThunk: () => A =
      fa.runFree([x] => (c: Console[x]) => c.toThunk)

    def toPar: Par[A] =
      fa.runFree([x] => (c: Console[x]) => c.toPar)

    def toReader: ConsoleReader[A] =
      fa.runFree([x] => (c: Console[x]) => c.toReader)

    def unsafeRunConsole: A =
      fa.translate([x] => (c: Console[x]) => c.toThunk).runTrampoline

case class ConsoleReader[A](run: String => A):
  def map[B](f: A => B): ConsoleReader[B] =
    ConsoleReader(r => f(run(r)))
  def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
    ConsoleReader(r => f(run(r)).run(r))

object ConsoleReader:
  given monad: Monad[ConsoleReader] with
    def unit[A](a: => A) = ConsoleReader(_ => a)
    extension [A](fa: ConsoleReader[A])
      override def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B]= fa.flatMap(f)

