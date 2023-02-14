package chapter6

import util.Utils.*
import scala.annotation.tailrec
import State._

trait RNG:
    def nextInt: (Int, RNG)

case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)

object Rand:
    type Rand[+A] = State[RNG, A]

    val int: Rand[Int] = State(_.nextInt)

    val nonNegativeInt: Rand[Int] = int map {
        case Int.MinValue => 0
        case n if n < 0 => -n
        case n => n
    }

    val double: Rand[Double] = nonNegativeInt map { i =>
        -i.toDouble / Int.MinValue.toDouble
    }

    val intDouble: Rand[(Int, Double)] =
        map2(int, double) { (_, _) }

    val doubleInt: Rand[(Double, Int)] =
        map2(double, int) { (_, _) }

    val double3: Rand[(Double, Double, Double)] =
        for
            d1 <- double
            d2 <- double
            d3 <- double
        yield (d1, d2, d3)

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
        map2(ra, rb) { (_, _) }

    val nonNegEvenInt: Rand[Int] =
        map(nonNegativeInt) { i => i - (i % 2) }

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

    val rng: RNG = SimpleRNG(42)

    @tailrec
    def printNRngs[A](f: Rand[A], rng: RNG = rng, times: Int = 5): Unit =
        if times > 0 then
            val (a, next) = f(rng)
            p(a)
            printNRngs(f, next, times - 1)

    @main
    def rngs(): Unit =
        //printNRngs(rng, double)
        printNRngs(ints(10))
        //p(double(SimpleRNG(42)))

