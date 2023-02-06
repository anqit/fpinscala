package chapter2

import util.Utils.p

import scala.annotation.tailrec

case object Chapter2:
    def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
        @tailrec
        def go(i: Int): Boolean =
            if i >= as.length - 1 then true
            else if gt(as(i + 1), as(i)) then go(i + 1)
            else false

        go(0)

    def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

    def curry2[A, B, C](f: (A, B) => C): A => B => C = a => f(a, _)

    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

    def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

    @main
    def test(): Unit =
        p(isSorted(Array(1, 2, 3), _ > _))
        p(isSorted(Array(1, 2, 1), _ > _))
        p(isSorted(Array(3, 2, 1), _ < _))
        p(isSorted(Array(1, 2, 3), _ < _))