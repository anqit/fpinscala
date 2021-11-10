package Chapter10

import Chapter8.{ Gen, Prop }

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

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
        Gen.forAll(gen.listOfN(3), "monoid associativity") {
            case a1 :: a2 :: a3 :: Nil =>
                m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))
        } && Gen.forAll(gen, "monoid idenity") { a =>
            m.op(a, m.zero) == a && m.op(m.zero, a) == a
        }
    }
}
