package Chapter6

trait RNG {
    def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        
        (n, nextRNG)
    } 
}

object RNG {
    type Rand[+A] = RNG => (A, RNG)

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        rng.nextInt match {
            case (x, r) if x >= 0 => (x, r)
            case (y, r) if y == Int.MinValue => (0, r)
            case (z, r) => (-z, r)
        }
    }

    def double(rng: RNG): (Double, RNG) = {
        val (n, rng2) = nonNegativeInt(rng)
        
        (n.toDouble / (Int.MaxValue.toDouble + 1), rng2)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (i, rng2) = nonNegativeInt(rng)
        val (d, rng3) = double(rng2)

        ((i, d), rng3)
    }
    
    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val (d, rng2) = double(rng)
        val (i, rng3) = nonNegativeInt(rng2)

        ((d, i), rng3)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (d1, rng2) = double(rng)
        val (d2, rng3) = double(rng2)
        val (d3, rng4) = double(rng3)

        ((d1, d2, d3), rng4)
    }
    
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        val buf = new collection.mutable.ListBuffer[Int]

        @annotation.tailrec
        def go(c: Int, r: RNG): (List[Int], RNG) = c match {
            case x if x > 0 =>
                val (nextInt, nextRNG) = r.nextInt
                buf += nextInt
                go(c - 1, nextRNG)
            case _ => (buf.toList, r)
        }

        go(count, rng)
    }

    val int: Rand[Int] = _.nextInt
    def unit[A](a: A): Rand[A] = rng => (a, rng)
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def doubleViaMap: Rand[Double] =
        map(nonNegativeInt)( _.toDouble / (Int.MaxValue.toDouble + 1))

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
            val (a, rng2) = ra(rng)
            val (b, rng3) = rb(rng2)
            (f(a, b), rng3)
        }

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
        map2(ra, rb)((_, _))

    def randIntDouble: Rand[(Int, Double)] = both(int, double)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
            var r = rng

            (for(
                f <- fs
            ) yield {
                val (a, next) = f(r)
                r = next
                a
            }, r)
        }


    def sequence_rec[A](fs: List[Rand[A]]): Rand[List[A]] = {
        def go(as: List[Rand[A]], rng: RNG): (List[A], RNG) = {
            as match {
                case Nil => (List(), rng)
                case ra :: tail =>
                    val (a, next) = ra(rng)
                    val (rest, last) = go(tail, next)
                    (a :: rest, last)
            }
        }
        rng => go(fs, rng)
    }

    def sequence_book[A](fs: List[Rand[A]]): Rand[List[A]] =
        fs.foldRight(unit(List[A]()))((ra, acc) => map2(ra, acc)(_ :: _))

    def ints_seq(count: Int): Rand[List[Int]] =
        sequence(List.fill(count)(int))

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
        val (a, next) = f(rng)
        g(a)(next)
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
        def retry(i: Int): Boolean = i > Int.MaxValue - Int.MaxValue % n

        flatMap(nonNegativeInt)(i => if(retry(i)) nonNegativeLessThan(n) else unit(i % n))
    }

    def mapViaFlatMap[A, B](ra: Rand[A])(f: A => B): Rand[B] = {
        flatMap(ra)(a => unit(f(a)))
    }

    def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra)(a => mapViaFlatMap(rb)(f(a, _)))

}

object Main {
    def main(args: Array[String]): Unit = {
        println("ints: " + RNG.ints(5)(SimpleRNG(42))._1)
        println("randIntDouble: " + RNG.randIntDouble(SimpleRNG(42))._1)
        println("seq: " + RNG.sequence(List(RNG.int, RNG.double, RNG.double, RNG.double))(SimpleRNG(42))._1)
        println("seq_rec: " + RNG.sequence_rec(List(RNG.int, RNG.double, RNG.double, RNG.double))(SimpleRNG(42))._1)
    }
}
