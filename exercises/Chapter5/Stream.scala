package Chapter5

sealed trait Stream[+A] {
    import Stream._

    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
        case Empty => this
        case Cons(h, t) => if(n > 0) Cons(() => h(), () => t().take(n - 1)) else Empty
    }

    def find(p: A => Boolean): Option[A] = this match {
        case Empty => None
        case Cons(h, t) => if(p(h())) Some(h()) else t().find(p)
    }
     
    def take_tr(n: Int): Stream[A] = {
        val buf = new collection.mutable.ListBuffer[A]
        
        @annotation.tailrec
        def go(s: Stream[A], i: Int): Stream[A] = s match {
            case Cons(h, t) if i > 0 =>
                buf += h()
                go(t(), i - 1)
            case _ => Stream(buf.toList: _*)
        }

        go(this, n)
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
        case Empty => this
        case Cons(h, t) => if(n > 0) t() drop (n - 1) else this
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
        val buf = new collection.mutable.ListBuffer[A]

        @annotation.tailrec
        def go(s: Stream[A], p: A => Boolean): Stream[A] = s match {
            case Cons(h, t) if p(h()) =>
                buf += h()
                go(t(), p)
            case _ => Stream(buf.toList: _*)
        }

        go(this, p)
    }

    @annotation.tailrec
    final def forAll(p: A => Boolean): Boolean = this match {
        case Cons(h, t) => p(h()) && t().forAll(p)
        case _ => true 
    }

    def forAll_fr(p: A => Boolean): Boolean =
        foldRight(true)((a, b) => p(a) && b)
        
    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def takeWhile_fr(p: A => Boolean): Stream[A] =
        foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else empty) 

    def headOption: Option[A] =
        foldRight(scala.None: Option[A])((a, _) => scala.Some(a))

    def map[B](f: A => B): Stream[B] = 
        foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter(p: A => Boolean): Stream[A] =
        foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

    def append[B >: A](bs: => Stream[B]): Stream[B] =
        foldRight(bs)((a, b) => cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(empty[B])((a, b) => f(a) append b)

    def map_u[B](f: A => B): Stream[B] = unfold(this) {
            case Empty => scala.None
            case Cons(h, t) => scala.Some(f(h()) -> t())
        }

    def take_u(n: Int): Stream[A] = unfold((this, n)) {
            case (Cons(h, t), n) if n > 0 => Some(h() -> (t(), n - 1))
            case _ => None
        }

    def takeWhile_u(p: A => Boolean): Stream[A] = unfold(this) {
            case Cons(h, t) if p(h()) => Some(h() -> t()) 
            case _ => None
        }

    
    def zip[B](bs: Stream[B]): Stream[(A, B)] =
        zipWith_u(bs)((_, _))

    def zipWith_u[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, bs)) {
            case (Empty, _) | (_, Empty) => None
            case (Cons(ah, at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
        }

    def zipAll_u[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, bs)) {
            case (Cons(ah, at), Cons(bh, bt)) => Some(((Some(ah()), Some(bh())), (at(), bt())))
            case (Cons(ah, at), Empty) => Some(((Some(ah()), None), (at(), empty[B])))
            case (Empty, Cons(bh, bt)) => Some(((None, Some(bh())), (empty[A], bt())))
            case (Empty, Empty) => None
        }

    def startsWith[A](s: Stream[A]): Boolean =
        zipAll_u(s).takeWhile(!_._2.isEmpty) forAll { case (h, h2) => h == h2 }

    def tails: Stream[Stream[A]] = unfold(this) {
            case Empty => None
            case s @ Cons(_, t) => Some((s, t()))
        } append Stream(empty)

    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
        tails.map(_.foldRight(z)(f))

    def hasSubsequence[A](s: Stream[A]): Boolean = 
        tails exists (_ startsWith s)

    override def toString = toList.toString
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl

        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
        if(as.isEmpty) empty else cons(as.head, Stream(as.tail: _*))

    def constant[A](a: A): Stream[A] = {
        lazy val tail: Stream[A] = cons(a, tail)
        tail

        // this match {
        //     case Empty => empty[B]
        //     case Cons(h, t) => cons(f(h()), t() map f)
        //  }

        // cons(a, constant(a))
    }

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs: Stream[Int] = {
        def go(cur: Int, next: Int): Stream[Int] = {
            cons(cur, go(next, cur + next))
        }

        go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z) match {
            case None => empty
            case Some((a, s)) => cons(a, unfold(s)(f))
        }
    }

    def fibs_u: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
    
    def from_u(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

    def constant_u[A](a: A): Stream[A] = unfold(a)(s => Some((a, a)))

    def ones_u = constant(1)
}

object Main {
    def main(args: Array[String]) = {
        val s = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)

        println("stream as list: " + s.toList)
        println("take 5: " + (s take 5))
        println("drop 3: " + (s drop 3))
        println("take 5 tail rec: " + (s take_tr 5))
        println("take while: " + (s takeWhile(_ < 4)))
        println("forAll even: " + s.forAll(_ % 2 == 0))
        println("forAll_fr even: " + s.forAll_fr(_ % 2 == 0))
        println("forAll lt 10: " + s.forAll(_ < 10))
        println("forAll_fr lt 10: " + s.forAll_fr(_ < 10))
        println()
        println("takeWhile_fr: " + s.takeWhile_fr(_ < 4))
        println("headOption: " + s.headOption)
        println()
        println("===== 5.7 =====")
        println("map: " + s.map(digitToString _))
        println("filter: " + s.filter(_ % 2 == 0))
        println("flatMap: " + s.flatMap(a => Stream((1 to a): _*)))

        println("constant: " + Stream.constant("hi").take(5))
        println("from: " + Stream.from(7).take(5))
        println("fibs: " + Stream.fibs.take(10))
        println()
        println("===== unfold =====")
        println("fibs unfold: " + Stream.fibs_u.take(10))
        println("from unfold: " + Stream.from_u(7).take(5))
        println("constant unfold: " + Stream.constant_u(7.9).take(6))
        println("ones unfold: " + Stream.ones_u.take(4))
        println("map_u: " + s.map_u(digitToString _))
        println("take_u: " + s.take_u(5))
        println("takeWhile_u: " + s.takeWhile_u(_ < 4))
        
        val bs = Stream("a","n","k","i","t")
        println("bs: " + bs)
        println("zipwith_u: " + s.zipWith_u(bs)((i, c) => c + digitToString(i)))
        println("zipAll_u: " + s.zipAll_u(bs))
        println()
        println("===== subs =====")
        println("startsWith ank: " + bs.startsWith(Stream("a", "n", "k")))
        println("startsWith arm: " + bs.startsWith(Stream("a", "r", "m")))
        println("tails: " + bs.tails)
        println("hasSub: " + bs.hasSubsequence(Stream("k", "i", "t")))

        println("scan right: " + Stream(1,2,3).scanRight(0)(_ + _))
    }

    def digitToString(i: Int): String = i match {
        case 0 => "zer0"
        case 1 => "1ne"
        case 2 => "2wo"
        case 3 => "thre3"
        case 4 => "4our"
        case 5 => "5ive"
        case 6 => "s6x"
        case 7 => "se7en"
        case 8 => "ei8ht"
        case 9 => "n9ne"
        case _ => "not a digit"
    }
}
