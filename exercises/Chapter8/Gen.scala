package Chapter8

import Chapter5.Stream
import Chapter6.{RNG, SimpleRNG, State}
import Chapter8.Prop.{FailedCase, SuccessCount, TestCases}

// trait Gen[A]

case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen(sample.flatMap(a => f(a).sample))

    def listOfN(size: Gen[Int]): Gen[List[A]] =
        size.flatMap(Gen.listOfN(_, this))
}

object Gen {
    def listOf[A](a: Gen[A]): Gen[List[A]] = ???

    def forAll[A](a: Gen[A])(f: A => Boolean, tag: String = ""): Prop = Prop { (n, rng) =>
        randomStream(a)(rng).zip(Stream.from(0)).take(n).map {
            case (a, i) => try {
                if (f(a)) Passed else Falsified(a.toString, i, tag)
            } catch { case e: Exception => Falsified(buildMessage(a, e), i, tag)}
        }.find(_.isFalsified).getOrElse(Passed)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
        Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMessage[A](s: A, e: Exception): String =
        s"test case: $s\n" +
        s"caused exception: ${e.getMessage}\n" +
          s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def choose(start: Int, stopExcl: Int): Gen[Int] = {
        val rand = RNG.map(RNG.nonNegativeLessThan(stopExcl - start)) { _ + start }

        Gen(State(s => rand(s)))
    }

    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    def boolean: Gen[Boolean] = {
        val rand = RNG.map(RNG.nonNegativeInt(_)) { _ % 2 == 0 }

        Gen(State(s => rand(s)))
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
        boolean.flatMap(b => if(b) g1 else g2)

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
        val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs) * Double.MaxValue

        Gen(State(RNG.double(_)) flatMap { d =>
          if(d < g1Threshold) g1._1.sample else g2._1.sample
        })
    }
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

case class Prop(run: (TestCases, RNG) => Result, tag: String = "") {
    def &&(p: Prop): Prop = Prop { (n, rng) =>
        run(n, rng) match {
            case f: Falsified => f
            case Passed => p.run(n, rng)
        }
    }

    def ||(p: Prop): Prop = Prop { (n, rng) =>
        run(n, rng) match {
            case Falsified(_, _, tag) => (p.copy(tag = tag)).run(n, rng)
            case x => x
        }
    }
}

object Prop {
    type TestCases = Int
    type SuccessCount = Int
    type FailedCase = String
}
