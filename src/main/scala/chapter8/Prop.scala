package chapter8

import chapter5.LazyList
import chapter6.{ RNG, SimpleRNG }

import scala.annotation.targetName

opaque type MaxSize = Int
object MaxSize:
    def fromInt(i: Int): MaxSize = i

    extension (self: MaxSize)
        def toInt: Int = self

opaque type SuccessCount = Int
opaque type FailedCase = String

enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)

    def isFalsified: Boolean = this match
        case Passed => false
        case _: Falsified => true

opaque type TestCases = Int
object TestCases:
    extension (x: TestCases)
        def toInt: Int = x

    def fromInt(x: Int): TestCases = x

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
    import Gen.*
    import Result.*

    def forAll[A](ga: Gen[A])(f: A => Boolean): Prop = (_, testCases, rng) =>
        randLazyList(ga)(rng).zip(LazyList.from(0)).take(testCases) map {
            case (a, i) =>
                try {
                    if f(a) then Passed else Falsified(a.toString, i)
                } catch
                    case e: Exception => Falsified(buildMsg(a, e), i)
        } find { _.isFalsified } getOrElse Passed

    @targetName("forAllSized")
    def forAll[A](sg: SGen[A])(f: A => Boolean): Prop = (maxSize, testCases, rng) =>
        val casesPerSize = (testCases - 1) / maxSize + 1
        val props: LazyList[Prop] =
            LazyList.from(0)
              .take((testCases min maxSize) + 1)
              .map { i => forAll(sg(i))(f) }
        val prop: Prop =
            props.map[Prop] { p => (maxSize, _, rng) =>
                p(maxSize, casesPerSize, rng)
            }
              .toList
              .reduce { _ && _ }
        prop(maxSize, testCases, rng)


    def randLazyList[A](ga: Gen[A])(rng: RNG): LazyList[A] =
        LazyList.unfold(rng)(r => Some(ga(r)))

    def buildMsg[A](s: A, e: Exception): String =
        s"test case: $s\n" +
          s"generated an exception: ${ e.getMessage }\n" +
          s"stack trace:\n ${ e.getStackTrace.mkString("\n") }"

    extension (self: Prop)
        @targetName("and")
        def &&(that: Prop): Prop = (n, cases, rng) => self(n, cases, rng) match
            case Passed => that(n, cases, rng)
            case f: Falsified => f

        @targetName("or")
        def ||(that: Prop): Prop = (n, cases, rng) => self(n, cases, rng) match
            case Passed => Passed
            case _: Falsified => that(n, cases, rng)

        def check(maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Result =
            self(maxSize, testCases, rng)

        def run(maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
            self(maxSize, testCases, rng) match
                case Falsified(msg, successes) =>
                    println(s"! Falsified after $successes tests:\n\t$msg")
                case Passed =>
                    println(s"+ OK, passes $testCases tests.")