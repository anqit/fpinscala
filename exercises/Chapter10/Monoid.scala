package Chapter10

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

    case class sorterthing[A] private (interval: Option[IndexedSeq[A]])(implicit ev: Ordering[A]) {
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

    def apply[A](o: (A, A) => A, z: A): Monoid[A] = new Monoid[A] {
        override def op(a1: A, a2: A): A = o(a1, a2)
        override def zero: A = z
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
    }
}
