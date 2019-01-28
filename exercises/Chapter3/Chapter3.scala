object Chapter3 {
    def exercise1 = List(1, 2, 3, 4, 5)  match {
        case x :: 2 :: 4 :: _ => x
        case Nil => 42
        case x :: y :: 3 :: 4:: _ => x + y
        case h :: t => h + t.sum
        case _ => 101
    }

    def tail[A](as: List[A]): List[A] = as match {
        case Nil => Nil
        case _ :: t => t
    }

    def maybeTail[A](as: List[A]): Option[List[A]] = as match {
        case Nil => None
        case _ :: t => Some(t)
    }

    def setHead[A](as: List[A], a: A): Option[List[A]] = maybeTail(as).map(a :: _)

    def drop[A](l: List[A], n: Int): List[A] = l match {
        case h :: t if n > 0 => drop(t, n - 1)
        case _ => l
    }
    
    def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
        case h :: t if p(h) => dropWhile(t, p)
        case _ => l
    }

    def init[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case a :: Nil => Nil
        case h :: t => h :: init(t)
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case h :: t => f(h, foldRight(t, z)(f))
    }

    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
        case Nil => z
        case x :: xs => foldLeft(xs, f(z, x))(f) 
    }

    def len[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

    def sumL(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
    def productL(as: List[Int]): Int = foldLeft(as, 1)(_ * _)
    def lenL[A](as: List[A]): Int = foldLeft(as, 0)((b, _) => b + 1)
    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => a :: b)

   //  def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
   //      as match {
   //          case Nil => z
   //          case x :: xs => 
   //      }
   //  }

    def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
    def foldLeftR[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, (b: B) => b)((a, g) => b =>g(f(b, a)))(z)

    def append[A](as: List[A], a: A): List[A] = {
       foldRight(as, a::Nil)((a, b) => a :: b) 
    }

    def append[A](as: List[A], bs: List[A]): List[A] = foldRight(as, bs)((a, b) => a::b)
    
    def concat[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil: List[A])(append)
    
    def add1(as: List[Int]): List[Int] = as match {
        case Nil => Nil
        case x :: xs => x + 1 :: add1(xs)
    }

    def string(as: List[Double]): List[String] = as match {
        case Nil => Nil
        case x :: xs => x.toString :: string(xs)
    }
    
    def map[A, B](as: List[A])(f: A => B): List[B] = as match {
        case Nil => Nil
        case x :: xs => f(x) :: map(xs)(f)
    }

    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
        case Nil => Nil
        case x :: xs if f(x) => x :: filter(xs)(f)
        case x :: xs => filter(xs)(f)
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = 
        concat(map(as)(f))
    // as match {
    //      case Nil => Nil
    //      case x :: xs => append(f(x), flatMap(xs)(f))
    //  }
    
    def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = 
        flatMap(as)(a => if(f(a)) List(a) else List()) 

    def sumLists(as: List[Int], bs: List[Int]): List[Int] = 
        as match {
            case Nil => Nil
            case x :: xs => (x + bs.head) :: sumLists(xs, tail(bs))
        }

    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
        case (_, Nil) => Nil
        case(Nil, _) => Nil
        case(x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
    }

    @annotation.tailrec
    def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
        (as, sub) match {
            case(_, Nil) => true
            case (Nil, _) => false
            case(x :: xs, y :: ys) => if(x == y) hasSubsequence(xs, ys) else hasSubsequence(xs, sub)
        }
    }


    def main(args: Array[String]) {
        println(exercise1)
        println(tail(List('a', 'b', 'c', 'd')))
        println(maybeTail(Nil))
        println(maybeTail(List(9,8,7,6,5)))

        println("=== Set head ===")
        val as = List(1, 2, 3, 4, 5)
        println(as)
        println(setHead(as, 7))
        println(setHead(Nil, 2))

        println("=== Drop ===")
        println(drop(as, -7))
        println(dropWhile(as, (a: Int) => a < 3))

        println("Init")
        println(init(as))

        println("=== folds ===")
        println("length: " + len(as))
        println("reverse: " + reverse(as))
        println("sumL: " + sumL(as))
        println("productL: " + productL(as))
        println("lenL: " + lenL(as))
        val bs = List(6, 7, 8, 9, 10)
        println("append: " + append(as, bs))
        println("concat: " + concat(List(as, bs)))
        println("add1: " + add1(as))
        println("string: " + string(as.map(_.toDouble)))
        println("map: " + map(as)(a => a*a))
        println("filter: " + filter(as)(_ > 2))
        println("fMap: " + flatMap(as)(a => List(a, a)))
        println("filterViaFM: " + filterViaFlatMap(as)(_ % 2 == 0))
        println("zipSum: " + sumLists(as, bs))
        val cs = List("hi", "bye", "oops", "word")
        println("zipWith: " + zipWith(as, cs)((a, c) => (a + c.length).toDouble))
        println("=== subseqs ===")
        println("(1,2): " + hasSubsequence(as, 1::2::Nil))
        println("(4): " + hasSubsequence(as, 4::Nil))
        println("(5): " + hasSubsequence(as, 5::Nil))
        println("(2,3): " + hasSubsequence(as, 2::3::Nil))
        println("(9, 10): " + hasSubsequence(as, 9::10::Nil))
        println("(5, 6): " + hasSubsequence(as, 5::6::Nil))
    }
}

