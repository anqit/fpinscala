package Chapter8

import Chapter5.Stream
import Chapter6.{RNG, SimpleRNG, State}
import Chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

// trait Gen[A]

case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen(sample.flatMap { f(_).sample })

    def listOfN(size: Gen[Int]): Gen[List[A]] =
        size.flatMap(Gen.listOfN(_, this))

    def unsized: SGen[A] = SGen { _ => this }
}

object Gen {
    def listOf[A](a: Gen[A]): Gen[List[A]] = ???

    def forAll[A](a: Gen[A])(f: A => Boolean, tag: String): Prop =
        Prop (
            (max, n, rng) =>
                randomStream(a)(rng).zip(Stream.from(0)).take(n).map {
                    case (a, i) => try {
                        if (f(a)) Passed else Falsified(a.toString, i, tag)
                    } catch {
                        case e: Exception => Falsified(buildMessage(a, e), i, tag)
                    }
                }.find(_.isFalsified).getOrElse(Passed),
            tag,
        )

    def forAll[A](a: SGen[A])(f: A => Boolean, tag: String): Prop =
        forAll(a(_))(f, tag)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean, tag: String): Prop = Prop { (max, n, rng) =>
        val casesPerSize = (n + max - 1) / max
        val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f, tag))
        val prop: Prop =
            props.map(p =>
                Prop((max, _, rng) => p.run(max, casesPerSize, rng), tag)
            ).toList.reduce(_ && _)

        prop.run(max, n, rng)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
        Stream.unfold(rng) { n => Some(g.sample.run(n)) }

    def buildMessage[A](s: A, e: Exception): String =
        s"test case: $s\n" +
          s"caused exception: ${e.getMessage}\n" +
          s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def choose(start: Int, stopExcl: Int): Gen[Int] = {
        val rand = RNG.map(RNG.nonNegativeLessThan(stopExcl - start)) { _ + start }

        Gen(State { rand(_) })
    }

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = {
        val rand = RNG.map(RNG.nonNegativeInt(_)) { _ % 2 == 0 }

        Gen(State { rand(_) })
    }

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
        val sample: RNG => (List[A], RNG) = rng => {
            var next = rng
            var list = List[A]()
            for {
                _ <- 1 to n
            } {
                val (a, s) = g.sample.run(next)
                next = s
                list = a :: list
            }

            (list, next)
        }

        Gen(State(sample))
    }

    def listOfN_book[A](n: Int, g: Gen[A]): Gen[List[A]] =
        Gen(State.sequence(List.fill(n)(g.sample)))

    def map[A, B](g: Gen[A])(f: A => B): Gen[B] = Gen(g.sample.map(f))

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
        boolean.flatMap { if(_) g1 else g2 }

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
        val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs) * Int.MaxValue.toDouble

        Gen(State(RNG.double(_)) flatMap { n =>
          if(n < g1Threshold) g1._1.sample else g2._1.sample
        })
    }
}

case class SGen[A](forSize: Int => Gen[A]) {
    def apply(n: Int): Gen[A] = forSize(n)

    def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { n =>
        forSize(n) flatMap {
            f(_).forSize(n)
        }
    }

    def map[B](f: A => B): SGen[B] = SGen { n =>
      Gen.map(forSize(n))(f)
    }
}

object SGen {
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen { n =>
        Gen.listOfN(n, g)
    }

    def forAll[A](sg: SGen[A])(f: A => Boolean, tag: String): Prop =
        Gen.forAll(sg(_))(f, tag)
}

sealed trait Result {
    def isFalsified: Boolean
}
case object Passed extends Result {
    override def isFalsified: Boolean = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount, tag: String = "") extends Result {
    override def isFalsified: Boolean = true
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result, tag: String = "") {
    def &&(p: Prop): Prop = Prop { (maxSize, n, rng) =>
        run(maxSize, n, rng) match {
            case f: Falsified => f
            case Passed => p.run(maxSize, n, rng)
        }
    }

    def ||(p: Prop): Prop = Prop { (maxSize, n, rng) =>
        run(maxSize, n, rng) match {
            case Falsified(_, _, tag) => (p.copy(tag = tag)).run(maxSize, n, rng)
            case x => x
        }
    }
}

object Prop {
    type TestCases = Int
    type SuccessCount = Int
    type FailedCase = String
    type MaxSize = Int

    def run(p: Prop,
            maxSize: MaxSize = 100,
            testCases: TestCases = 100,
            rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
        p.run(maxSize, testCases, rng) match {
            case Falsified(msg, n, tag) => println(s"! falsified prop ${p.tag} after $n passed tests:\n\t/$msg")
            case Passed => println(s"+ OK, prop ${p.tag} passed $testCases tests")

        }
}
