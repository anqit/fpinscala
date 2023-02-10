package chapter4

import util.Utils.p

enum Option[+A]:
    case Some(get: A)
    case None

    def map[B](f: A => B): Option[B] = this match
        case Some(a) => Some(f(a))
        case None => None

    def flatMap[B](f: A => Option[B]): Option[B] =
        map(f).getOrElse(None)

    def getOrElse[B >: A](default: => B): B = this match
        case Some(b) => b
        case None => default

    def orElse[B >: A](ob: => Option[B]): Option[B] =
        map(_ => this).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] =
        flatMap(a => if (f(a)) this else None)

    def fold[B](b: B)(f: (A, B) => B): B =
        map(f(_, b)).getOrElse(b)

object Option:
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a flatMap { x => b map { y => f(x, y) } }

    def sequence[A](as: List[Option[A]]): Option[List[A]] =
        traverse(as) { identity }

    def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
        as.foldRight(Some(Nil): Option[List[B]]) { (a, acc) => map2(f(a), acc) { _ :: _}}

    def lift[A, B](f: A => B): Option[A] => Option[B] =
        _.map(f)

    def toIntOption(s: String): Option[Int] =
        try Some(s.toInt)
        catch case _: NumberFormatException => None

    def mean(xs: Seq[Double]): Option[Double] =
        if xs.isEmpty then None
        else Some(xs.sum / xs.length)

    // math.pow(x - m, 2)
    def variance(xs: Seq[Double]): Option[Double] =
        mean(xs) flatMap { m => mean(xs.map(x => math.pow(x - m, 2))) }

    @main
    def options(): Unit =
        p(variance(List(2, 4, 4, 4, 5, 5, 7, 9)))
