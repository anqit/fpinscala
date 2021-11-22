package Chapter10

import Chapter3.{ Branch, Leaf, Tree }
import Chapter6.State
import Chapter7.Par
import Chapter7.Par.{ asyncF, Par }
import Chapter8.{ Gen, Prop }
import Chapter9.ParsersImpl.asStringParser

trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
}

object Monoid {
    val stringConcatMonoid = new Monoid[String] {
        override def op(a1: String, a2: String): String = a1 + a2
        override def zero: String = ""
    }

    def listConcatMonoid[A] = new Monoid[List[A]] {
        override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
        override def zero: List[A] = Nil
    }

    val intAddition = new Monoid[Int] {
        override def op(a1: Int, a2: Int): Int = a1 + a2
        override def zero: Int = 0
    }

    val intMultiplication = new Monoid[Int] {
        override def op(a1: Int, a2: Int): Int = a1 * a2
        override def zero: Int = 1
    }

    val booleanOr = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
        override def zero: Boolean = false
    }

    val booleanAnd = new Monoid[Boolean] {
        override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
        override def zero: Boolean = true
    }

    def optionMonoid[A] = new Monoid[Option[A]] {
        override def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
            case (None, _) => a2
            case _ => a1
        }

        override def zero: Option[A] = None
    }

    def endoMonoid[A] = new Monoid[A => A] {
        override def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
        override def zero: A => A = identity(_)
    }

    def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
        override def op(a1: A, a2: A): A = m.op(a2, a1)
        override def zero: A = m.zero
    }

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
        Gen.forAll(gen.listOfN(3), "monoid associativity") {
            case a1 :: a2 :: a3 :: Nil =>
                m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))
            case _ => false // shouldn't happen
        } && Gen.forAll(gen, "monoid idenity") { a =>
            m.op(a, m.zero) == a && m.op(m.zero, a) == a
        }
    }

    def foldMap[A, B](as: Seq[A], m: Monoid[B])(f: A => B): B =
        as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

    def isSorted[A](v: IndexedSeq[A])(implicit ev: Ordering[A]): Boolean = {
        val theMonoid = new Monoid[sorterthing[A]] {
            override def op(a1: sorterthing[A], a2: sorterthing[A]): sorterthing[A] = a1 combine a2

            override def zero: sorterthing[A] = sorterthing()
        }

        foldMapV(v, theMonoid)(sorterthing(_)).isSorted
    }

    case class sorterthing[A] private(interval: Option[IndexedSeq[A]])(implicit ev: Ordering[A]) {
        def combine(other: sorterthing[A]): sorterthing[A] = (interval, other.interval) match {
            case (Some(v1), Some(v2)) => (v1, v2) match {
                case (_, l) if l.isEmpty => sorterthing(Some(v1))
                case (l, _) if l.isEmpty => sorterthing(Some(v2))
                case (as :+ a, b +: bs) if ev.lteq(a, b) => sorterthing(Some(v1 ++ v2))
                case _ => sorterthing(None)
            }
            case _ => sorterthing(None)
        }

        def isSorted: Boolean = interval.isDefined
    }

    object sorterthing {
        def apply[A]()(implicit ev: Ordering[A]) = new sorterthing[A](Some(Vector()))
        def apply[A](a: A)(implicit ev: Ordering[A]) = new sorterthing[A](Some(Vector(a)))
    }

    def foldRight_viaFoldMap[A, B](as: List[A], zero: B)(f: (B, A) => B): B =
        foldMap(as, endoMonoid[B])(a => f(_, a))(zero)

    def foldLeft_viaFoldMap[A, B](as: List[A], zero: B)(f: (B, A) => B): B =
        foldMap(as, dual(endoMonoid[B]))(a => f(_, a))(zero)

    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
        case Nil =>
            m.zero
        case a +: Nil =>
            f(a) // equivalent to m.op(f(a), m.zero) by monoid laws
        case as =>
            val (l, r) = as.splitAt(as.length / 2)
            m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

    def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
        override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
        override def zero: Par[A] = Par.unit(m.zero)
    }

    def foldMapPar[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = v match {
        case Nil =>
            Par.unit(m.zero)
        case a +: Nil =>
            Par.map(asyncF(f)(a))(m.op(_, m.zero))
        case as =>
            val (l, r) = as.splitAt(as.length / 2)
            Par.map2(foldMapPar(l, m)(f), foldMapPar(r, m)(f))(m.op)
    }

    def productMonoid[A, B](a: Monoid[A], b: Monoid[B]) = Monoid[(A, B)] (
        (a1, a2)=> (a.op(a1._1, a2._1), b.op(a1._2, a2._2)),
        (a.zero, b.zero)
    )

    def functionMonoid[A, B](B: Monoid[B]) = Monoid[A => B](
        (a1, a2) => a => B.op(a1(a), a2(a)),
        _ => B.zero
    )

    def apply[A](o: (A, A) => A, z: A): Monoid[A] = new Monoid[A] {
        override def op(a1: A, a2: A): A = o(a1, a2)
        override def zero: A = z
    }

    def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
        val m = new Monoid[Map[A, Int]] {
            override def op(a1: Map[A, Int], a2: Map[A, Int]): Map[A, Int] = {
                a1.keySet.foldLeft(a2) { (acc, k) =>
                    acc.updated(k, acc.getOrElse(k, 0) + a1(k))
                }
            }

            override def zero = Map()
        }

        foldMapV(as, m)(a => Map(a -> 1))
    }

    def main(args: Array[String]) = {
        println(isSorted(Vector(1, 2, 3, 4)))
        println(isSorted(Vector[Int]()))
        println(isSorted(Vector(100)))
        println(isSorted(Vector(101, 100)))
        println(isSorted(Vector(1, 2, 3, 4, 1)))
        println(isSorted(Vector(1, 2, 3, 4, 1)))
        println(isSorted(Vector(1, 2, 3, 4, /**/ 5, 200, 201, 204, /**/ 9, 10, 11, 12, /**/ 13, 23, 31, 44)))
        // ^ tests sorterthing.combine { ... case (as :+ a, b +: bs) if ev.lteq(a, b) => sorterthing(Some(v1 :+ b)) ... }
        //                                                                                                   ^ wrong op
        println(bag(Vector("a", "rose", "is", "a", "rose")))
    }
}

object WordCount {
    sealed trait WC {
        def +(other: WC): WC

        def wc: Int
    }

    case class Stub(chars: String) extends WC {
        override def +(other: WC): WC = other match {
            case Stub(ochars) => Stub(chars + ochars)
            case Part(l, words, r) => Part(chars + l, words, r)
        }

        override def wc: Int = countWord(chars)
    }

    case class Part(lStub: String, words: Int, rStub: String) extends WC {
        override def +(other: WC): WC = other match {
            case Stub(ochars) => Part(lStub, words, rStub + ochars)
            case Part(ol, owords, or) => Part(lStub, words + countWord(rStub + ol) + owords, or)
        }

        override def wc: Int = countWord(lStub) + words + countWord(rStub)
    }

    val wcMonoid: Monoid[WC] = new Monoid[WC] {
        override def op(a1: WC, a2: WC): WC = a1 + a2

        override def zero: WC = Stub("")
    }

    def wc(s: String): Int = {
        def toWc(c: Char) = if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)

        Monoid.foldMapV(s.toCharArray, wcMonoid)(toWc _).wc
    }

    def countWord(s: String) = if (s.isEmpty) 0 else 1

    def main(args: Array[String]) = {
        println(wc("hi"))
        println(wc(""))
        println(wc("hi there guy"))
        println(wc("   \thi   there\n  guy     "))
    }
}

trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(m: Monoid[B]): B

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
        foldLeft(as)(m.zero)(m.op)

    def toList[A](as: F[A]): List[A] =
        foldLeft(as)(List[A]())((b, a) => a :: b)
}

object Foldable {
    val fList = new Foldable[List] {
        def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
            as.foldRight(z)(f)

        def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
            as.foldLeft(z)(f)

        def foldMap[A, B](as: List[A])(f: A => B)(m: Monoid[B]): B =
            as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
    }

    val fIdxSeq = new Foldable[IndexedSeq] {
        def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
            as.foldRight(z)(f)

        def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
            as.foldLeft(z)(f)

        def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(m: Monoid[B]): B =
            as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
    }

    val fStream = new Foldable[Stream] {
        def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
            as.foldRight(z)(f)

        def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
            as.foldLeft(z)(f)

        def foldMap[A, B](as: Stream[A])(f: A => B)(m: Monoid[B]): B =
            as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
    }

    val fTree = new Foldable[Tree] {
        def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
            case Leaf(value) => f(value, z)
            case Branch(left, right) =>
                val rb = foldRight(right)(z)(f)
                foldRight(left)(rb)(f)
        }

        def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
            case Leaf(value) => f(z, value)
            case Branch(left, right) =>
                val lb = foldLeft(left)(z)(f)
                foldLeft(right)(lb)(f)
        }

        def foldMap[A, B](as: Tree[A])(f: A => B)(m: Monoid[B]): B = as match {
            case Leaf(value) => f(value)
            case Branch(left, right) =>
                m.op(foldMap(left)(f)(m), foldMap(right)(f)(m))
        }
    }

    val fOption = new Foldable[Option] {
        override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
            as.foldRight(z)(f)

        override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
            case None => z
            case Some(value) => f(z, value)
        }

        override def foldMap[A, B](as: Option[A])(f: A => B)(m: Monoid[B]) = as match {
            case Some(value) => f(value)
            case None => m.zero
        }

    }
}
