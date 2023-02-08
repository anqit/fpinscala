package chapter3

import util.Utils._
import scala.annotation.{ tailrec, targetName }

enum List[+A]:
    case Nil
    case Cons(head: A, tail: List[A])

    @targetName("cons")
    def ::[B >: A](b: B): List[B] = Cons(b, this)

object List:
    def apply[A](as: A*): List[A] =
        if as.isEmpty then Nil
        else Cons(as.head, List(as.tail *))

    def sum(ints: List[Int]): Int = ints match
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)

    def product(doubles: List[Double]): Double = doubles match
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs) => x * product(xs)

    def tail[A](l: List[A]): List[A] = l match
        case Cons(_, tail) => tail
        case Nil => sys.error("empty list has no tail")

    def setHead[A](l: List[A], a: A): List[A] = l match
        case Cons(_, tail) => Cons(a, tail)
        case Nil => List(a)

    @targetName("in")
    def initial[A](as: List[A]): List[A] = as match
        case Nil | Cons(_, Nil) => Nil
        case Cons(a, tail) => Cons(a, initial(tail))

    @tailrec
    @targetName("drp")
    def drop[A](as: List[A], n: Int): List[A] = (as, n) match
        case (Nil, _) => Nil
        case (_, 0) => as
        case (Cons(_, tail), d) if d > 0 => drop(tail, n -1)
        case _ => as

    def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
        as match
            case Nil => acc
            case Cons(x, xs) => f(x, foldRight(xs, acc, f))

    def length(as: List[_]): Int = foldRight(as, 0, (_, acc) => 1 + acc)

    @tailrec
    def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B = as match
        case Nil => acc
        case Cons(h, tl) => foldLeft(tl, f(acc, h), f)

    def sumViaFL[A](as: List[A])(using num: Numeric[A]): A =
        foldLeft(as, num.zero, num.plus)

    def productViaFL[A](as: List[A])(using num: Numeric[A]): A =
        foldLeft(as, num.one, num.times)

    def lengthViaFL(as: List[_]): Int =
        foldLeft(as, 0, (acc, _) => acc + 1)

    def reverse[A](as: List[A]): List[A] =
        foldLeft(as, Nil: List[A], (acc, a) => a :: acc)

    def frViaFl[A, B](as: List[A], acc: B, f: (A, B) => B): B =
        foldLeft(reverse(as), acc, (b, a) => f(a, b))

    def append[A](a1: List[A], a2: List[A]): List[A] =
        foldRight(a1, a2, _ :: _)

    def concat[A](aas: List[List[A]]): List[A] =
        foldRight(aas, Nil: List[A], append)

    def map[A, B](as: List[A], f: A => B): List[B] =
        foldRight(as, Nil :List[B], (a, acc) => f(a) :: acc)

    def filter[A](as: List[A], f: A => Boolean): List[A] =
        foldRight(as, Nil: List[A], (a, acc) => if f(a) then a :: acc else acc)

    def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
        foldRight(as, Nil: List[B], (a, acc) => append(f(a), acc))

    def filterViaFM[A](as: List[A], f: A => Boolean): List[A] =
        flatMap(as, a => if f(a) then List(a) else Nil)

    def zip[A, B, C](as: List[A], bs: List[B], f: (A, B) => C): List[C] = (as, bs) match
        case (Cons(ah, at), Cons(bh, bt)) => f(ah, bh) :: zip(at, bt, f)
        case _ => Nil

    //@tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match
        case (Nil, Nil) => true
        case (Cons(ah, at), Cons(bh, bt)) if ah == bh && hasSubsequence(at, bt) => true
        case (Cons(_, at), _) => hasSubsequence(at, sub)
        case _ => false


    @tailrec
    @targetName("drpW")
    def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match
        case Nil => Nil
        case Cons(hd, tl) => if f(hd) then dropWhile(tl, f) else as

    extension [T](ts: List[T])
        def drop(d: Int): List[T] = List.drop(ts, d)
        def dropWhile(f: T => Boolean): List[T] = List.dropWhile(ts, f)
        def init: List[T] = List.initial(ts)

    import math.Numeric.Implicits.infixNumericOps
    extension [T](ts: List[T])(using n: Numeric[T])
        def sum: T = ts match
        case Nil => n.zero
        case Cons(x, xs) => x + xs.sum

        def product: T = ts match
            case Nil => n.one
            case Cons(x, xs) => x * xs.product

    @main
    def lists(): Unit =
        val is = List(1, 2, 3, 4, 5)
        val is2 = List(9, 10, 11, 12, 13)
        val iis = List(is2, is)
        //p(reverse(is))
        //p(append(is2, is))
        //p(iis)
        //p(concat(iis))
        //p(map(is, a => s"num$a"))
        //p(map(is, _ + 1))
        //p(filter(is, _ % 2 == 0))
        //p(flatMap(List(1, 2, 3), i => List(i,i)))
        //p(map(List(1, 2, 3), i => List(i,i)))
        //p(filterViaFM(is, _ % 2 == 0))
        p(zip(List(1,2,3), List(4,5,6), _ + _))
        p(hasSubsequence(List(1,1,2,3,4), List(1,2)))
        //p(hasSubsequence(List(1,2,3,4), List(2,3)))
        //p(hasSubsequence(List(1,2,3,4), List(4)))
        //p(hasSubsequence(List(1,2,3,4), List(3, 2)))
        //p(hasSubsequence(List(1,2,3,4), List(5)))


        //p(is.sum)
        //p(is.product)
        //val result = List(1, 2, 3, 4, 5) match
        //    case Cons(x, Cons(2, Cons(4, _))) => x
        //    case Nil => 42
        //    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        //    case Cons(h, t) => h + sum(t)
        //    case _ => 101
        //p(result)
        //b()
        //p(List.setHead(is, 10))
        //b()
        //p(is.drop(1))
        //p(is.drop(2))
        //p(is.drop(10))
        //p(is.drop(-5))
        //b()
        //p(is.dropWhile(_ % 2 == 0))
        //p(is.dropWhile(_ % 2 != 0))
        //p(is.dropWhile(_ < 4))
        //b()
        //p(is.init)
        //p(List.length(is))
        //p(List.productViaFL(is))
        //p(List.sumViaFL(is))
