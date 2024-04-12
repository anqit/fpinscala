package chapter8

import chapter5.LazyList
import chapter6.{ Rand, RNG, SimpleRNG, State }

import scala.annotation.{ tailrec, targetName }

opaque type Gen[+A] = State[RNG, A]
opaque type SGen[+A] = Int => Gen[A]

object Gen:
    //def listOf[A](g: Gen[A]): Gen[List[A]] = ???

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
        State { rng =>
            val (i, next) = rng.nextInt
            (i % (stopExclusive - start) + start, next)
        }

    def unit[A](a: => A): Gen[A] = State.unit(a)

    def boolean: Gen[Boolean] = State { rng =>
        val (i, next) = rng.nextInt
        (i % 2 == 0, next)
    }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap {
        case true => g1
        case false => g2
    }

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = (g1, g2) match
        case ((ga1, w1), (ga2, w2)) => Rand.double.flatMap { w => if w < w1 / (w1 + w2) then ga1 else ga2 }
        
    def map2[A, B, C](ga: Gen[A], gb: Gen[B])(f: (A, B) => C): Gen[C] =
        State.map2(ga, gb)(f)

    extension[A] (self: Gen[A])
        def apply(rng: RNG): (A, RNG) = State.apply(self)(rng)

        def listOfN(n: Int): Gen[List[A]] =
            @tailrec
            def go(c: Int, l: List[A], rng: RNG): (List[A], RNG) =
                if c > 0 then
                    val (a, next) = self(rng)
                    go(c - 1, a :: l, next)
                else
                    (l, rng)

            State { rng =>
                go(n, Nil, rng)
            }

        def flatMap[B](f: A => Gen[B]): Gen[B] =
            State.fm(self, f)

        def listOfNG(size: Gen[Int]): Gen[List[A]] =
            size flatMap listOfN

        def unsized: SGen[A] = _ => self

        def list: SGen[List[A]] = size => listOfN(size)

    extension[A] (sgen: SGen[A])
        def apply(i: Int): Gen[A] = sgen(i)

        @targetName("flatMapS")
        def flatMap[B](f: A => Gen[B]): SGen[B] = size =>
            sgen(size) flatMap f

        @targetName("listOfNS")
        def listOfN(n: Int): SGen[List[A]] = size =>
            sgen(size) listOfN n
            