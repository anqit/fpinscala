package chapter6

import util.Utils.*

import scala.annotation.tailrec


trait RNG:
    def nextInt: (Int, RNG)

object RNG:
    def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match
        case (Int.MinValue, next) => (0, next)
        case (n, next) if n < 0 => (-n, next)
        case (n, next) => (n, next)

    def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match
        case (i, next) => (-i.toDouble / Int.MinValue.toDouble, next)

    def intDouble(rng: RNG): ((Int, Double), RNG) =
        val (i, r2) = rng.nextInt
        val (d, r3) = double(r2)
        ((i, d), r3)

    def doubleInt(rng: RNG): ((Double, Int), RNG) =
        val ((i, d), next) = intDouble(rng)
        ((d, i), next)

    def double3(rng: RNG): ((Double, Double, Double), RNG) =
        val (d1, r1) = double(rng)
        val (d2, r2) = double(r1)
        val (d3, r3) = double(r2)
        ((d1, d2, d3), r3)

    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
        @tailrec
        def go(r: RNG, c: Int, l: List[Int]): (List[Int], RNG) =
            if c > 0 then
                val (i, next) = r.nextInt
                go(next, c - 1, i :: l)
            else (l, r)

        go(rng, count, Nil)


    val rng = SimpleRNG(42)

    @tailrec
    def printNRngs[A](f: RNG => (A, RNG), rng: RNG = rng, times: Int = 5): Unit =
        if times > 0 then
            val (a, next) = f(rng)
            p(a)
            printNRngs(f, next, times - 1)

    @main
    def rngs(): Unit =
        //printNRngs(rng, double)
        printNRngs(ints(10))
        //p(double(SimpleRNG(42)))

case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)

object Rand:
    import RNG._

    type Rand[+A] = RNG => (A, RNG)

    def unit[A](a: A): Rand[A] = r => (a, r)

    def map[A, B](r: Rand[A])(f: A => B): Rand[B] =
        flatMap(r) { a => unit(f(a)) }
        //  rng => {
        //      val (a, next) = r(rng)
        //      (f(a), next)
        //  }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra) { a => map(rb) { b => f(a, b) }}
        //  rng => {
        //      val (a, r1) = ra(rng)
        //      val (b, r2) = rb(r1)
        //
        //      (f(a, b), r2)
        //  }

    def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] =
        rng =>
          val (a, next) = ra(rng)
          f(a)(next)

    def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
        rs.foldRight(unit[List[A]](Nil)) { map2(_, _) { _ :: _ } }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
        map2(ra, rb) { (_, _) }

    def nonNegEvenInt: Rand[Int] =
        map(nonNegativeInt) { i => i - (i % 2) }

    def int: Rand[Int] = _.nextInt

    def double: Rand[Double] =
        map(nonNegativeInt) { i => -i.toDouble / Int.MinValue.toDouble }

    val randIntDouble: Rand[(Int, Double)] =
        both(int, double)

    val randDoubleInt: Rand[(Double, Int)] =
        both(double, int)

    def ints(count: Int): Rand[List[Int]] =
        sequence(List.fill(count)(int))

    def nonNegativeLessThan(max: Int): Rand[Int] =
        flatMap(nonNegativeInt) { i =>
            val mod = i % max
            if i + (max - 1) - mod >= 0 then
                unit(mod)
            else nonNegativeLessThan(max)
        }

    def rollDie: Rand[Int] = map(nonNegativeLessThan(6)) { _ + 1 }

    
