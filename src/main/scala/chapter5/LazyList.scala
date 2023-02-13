package chapter5

import util.Utils._

enum LazyList[+A]:
    import LazyList._

    case Empty
    case Cons(h: () => A, t: () => LazyList[A])

    def foldRight[B](acc: => B)(f: (A, => B) => B): B =
        this match
            case Cons(h, t) => f(h(), t().foldRight(acc)(f))
            case _ => acc

    def headOption: Option[A] =
        foldRight[Option[A]](None) { (a, _) => Some(a) }
        //this match
        //    case Cons(h, _) => Some(h())
        //    case Empty => None

    def toList: List[A] = this match
        case Cons(h, t) => h() :: t().toList
        case Empty => Nil

    def take(n: Int): LazyList[A] =
        unfold((this, n)) {
            case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
            case _ => None
        }
        //this match
        //    case Cons(hd, _) if == 1 => cons(hd(), empty) // <-- special case to prevent evaluating tail
        //    case Cons(hd, tl) if n > 1 => cons(hd(), tl().take(n - 1))
        //    case _ => empty

    def drop(n: Int): LazyList[A] = this match
        //case _ if n <= 0 => this
        case Cons(_, tl) if n > 0 => tl().drop(n - 1)
        case _ => this

    def takeWhile(p: A => Boolean): LazyList[A] =
        unfold(this) {
            case Cons(h, t) => if p(h()) then Some(h(), t()) else None
            case _ => None
        }
        //foldRight(empty) { (a, acc) => if p(a) then cons(a, acc) else empty }
        //this match
        //    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
        //    case Empty => empty

    def exists(p: A => Boolean): Boolean =
        foldRight(false) { (a, acc) => p(a) || acc }

    def forAll(p: A => Boolean): Boolean =
        foldRight(true) { (a, acc) => p(a) && acc }

    def map[B](f: A => B): LazyList[B] =
        unfold(this) {
            case Cons(h, t) => Some(f(h()), t())
            case _ => None
        }
        //foldRight(empty) { (a, acc) => cons(f(a), acc) }

    def filter(p: A => Boolean): LazyList[A] =
        foldRight(empty) { (a, acc) => if p(a) then cons(a, acc) else acc}

    def append[B >: A](bs: => LazyList[B]): LazyList[B] =
        foldRight(bs) { cons }

    def flatMap[B](f: A => LazyList[B]): LazyList[B] =
        foldRight(empty) { (a, acc) => f(a) append acc }

    def find(p: A => Boolean): Option[A] =
        filter(p).headOption

    def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
        unfold((this, that)) {
            case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
            case (Cons(ah, at), Empty) => Some((Some(ah()), None), (at(), empty))
            case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (empty, bt()))
            case _ => None
        }

    def startsWith[A](prefix: LazyList[A]): Boolean =
        zipAll(prefix).takeWhile(_(1).isDefined).forAll((a1, a2) => a1 == a2)
        //zipAll(prefix).map {
        //    case (_, None) => 1
        //    case (Some(a), Some(b)) if a == b => 0
        //    case _ => -1
        //}.find(_ != 0).contains(1)

    def tails: LazyList[LazyList[A]] =
        unfold(this) {
            case l @ Cons(_, t) => Some(l, t())
            case Empty => None
        } append LazyList(empty)

    def hasSubsequence[A](l: LazyList[A]): Boolean =
        tails.exists(_.startsWith(l))

    def scanRight[B](acc: => B)(f: (A, => B) => B): LazyList[B] =
        foldRight(LazyList(acc)) { (a, lists) => lists match {
            case Cons(h, _) => LazyList(f(a, h())) append lists
        }}


object LazyList:
    def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)

    def empty[A]: LazyList[A] = Empty

    def apply[A](a: => A): LazyList[A] =
        cons(a, empty)

    def apply[A](as: A*): LazyList[A] =
        if as.isEmpty then empty
        else cons(as.head, apply(as.tail*))

    def continually[A](a: A): LazyList[A] =
        unfold(()) { _ => Some((a, ())) }
        //lazy val cont: LazyList[A] = cons(a, cont)
        //cont

    def from(n: Int): LazyList[Int] =
        unfold(n) { s => Some(s, s + 1) }
        //cons(n, from(n + 1))

    def fibs: LazyList[Int] =
        unfold((0, 1)) { (a, b) => Some(a, (b, a + b)) }
        //def go(a: Int, b: Int): LazyList[Int] =
        //    cons(a, go(b, a + b))
        //go(0, 1)

    def zipWith[A, B, C](as: LazyList[A], bs: LazyList[B], f: (A, B) => C): LazyList[C] =
        unfold((as, bs)) {
            case (Cons(ah, at), Cons(bh, bt)) => Some(f(ah(), bh()), (at(), bt()))
            case _ => None
        }

    def unfold[A, S](s: S)(f: S => Option[(A, S)]): LazyList[A] = f(s) match
        case Some((a, next)) => cons(a, unfold(next)(f))
        case _ => empty

    def pll(l: LazyList[_], n: Int = 10): Unit =
        println(l.take(n).toList)

    @main
    def lazylists(): Unit =
        //val ll = LazyList(1,2,3).takeWhile(_ % 2 == 0)
        //pll(ll)

        lazy val ones: LazyList[Int] = LazyList.cons(1, ones)
        //pll(ones, 5)
        //pll(fibs)
        //pll(continually("do"))
        //pll(from(10), 19)
        //pll(from(10) map { _ * 4 } takeWhile (_ < 60))
        //pll(zipWith(fibs.take(10), continually("joe").take(15), (f, j) => s"$f$j"))
        //pll(fibs.take(10).zipAll(continually("joe").take(15)), 20)
        //pll(fibs.take(15).zipAll(continually("joe").take(10)), 20)
        //p(LazyList("10.0", "2.0", "2.0").foldRight("") { _ + _ })
        //p(LazyList(10, 5, 6).find(_ % 2 == 0))
        //p(LazyList(1, 2, 3).startsWith(LazyList(1, 2))) // true
        //p(LazyList(1, 2, 3).startsWith(LazyList(1, 2, 4))) // false
        //p(LazyList(1, 2, 3).startsWith(LazyList(1, 2, 3, 4))) // false
        //p(LazyList(1, 2, 3).startsWith(LazyList(2, 3))) // false
        //p(LazyList(1, 2, 3).startsWith(LazyList(0, 1, 2))) // false
        p(from(1) startsWith from(1).take(3))
        p(from(1).take(4) startsWith from(1))
        //p(ones.takeWhile(_ == 1).toList)
        pll(LazyList(1, 2, 3).tails map { _.toList })
        pll(LazyList(1, 2, 3).scanRight(0)(_ + _))
