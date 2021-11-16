package Chapter10

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

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
        as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

    def isSorted[A](v: IndexedSeq[A])(implicit ev: Ordering[A]): Boolean = {
        val extoMonoid = new Monoid[IndexedSeq[A] => Boolean] {
            override def op(a1: IndexedSeq[A] => Boolean, a2: IndexedSeq[A] => Boolean): IndexedSeq[A] => Boolean = {
                case l if l.isEmpty => zero(l)
                case l =>
                    val (l1, l2) = l.splitAt(l.length / 2)
                    a1(l1) && a2(l2)
            }

            override def zero: IndexedSeq[A] => Boolean = _ => true
        }

        def toIsSortedFunction(as: IndexedSeq[A]): IndexedSeq[A] => Boolean = {
            case l if l.length <= 1 => true
            case l =>
                val (l1, l2) = l.splitAt(l.length / 2)
                isSorted(l1) && isSorted(l2) && ev.lteq(l1.last, l2.head)
        }

        val (l, r) = v.splitAt(v.length / 2)
        foldMap(l :: r :: Nil, extoMonoid)()
    }

    def foldRight_viaFoldMap[A, B](as: List[A], zero: B)(f: (B, A) => B): B =
        foldMap(as, endoMonoid[B])(a => f(_, a))(zero)

    def foldLeft_viaFoldMap[A, B](as: List[A], zero: B)(f: (B, A) => B): B =
        foldMap(as, dual(endoMonoid[B]))(a => f(_, a))(zero)

    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v match {
        case Nil =>
            m.zero
        case a +: Nil =>
            m.op(f(a), m.zero)
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
}
