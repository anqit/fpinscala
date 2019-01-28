sealed trait MyOption[+A] {
    def map[B](f: A => B): MyOption[B] = None
    def flatMap[B](f: A => MyOption[B]): MyOption[B] = None
    def getOrElse[B >: A](default: => B): B = default
    def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = ob
    def filter(f: A => Boolean): MyOption[A] = None
}

case class Some[+A](get: A) extends MyOption[A] {
    override def map[B](f: A => B) = f(get) match {
        case Nil => None
        case b => Some(b)
    }

    override def flatMap[B](f: A => MyOption[B]) = f(get)

    override def getOrElse[B >: A](default: => B) = get

    override def orElse[B >: A](ob: => MyOption[B]) = this

    override def filter(f: A => Boolean) = if(f(get)) this else None
}

case object None extends MyOption[Nothing]

object MyOption {
    def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = a.flatMap(x => b.map(y => f(x, y)))

    def sequence[A](os: List[MyOption[A]]): MyOption[List[A]] =
        // if(os.contains(None)) None
        // else Some(os.map(o => o match { case Some(a) => a }))

        // os.foldRight[MyOption[List[A]]](Some(Nil: List[A]))((a, b) => map2(a, b)(_ :: _))

        os match {
            case Nil => Some(Nil) 
            case x :: xs => x flatMap(xx => sequence(xs) map (xx :: _))
        }

    def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] = 
        as.foldRight[MyOption[List[B]]](Some(Nil))((a, o) => map2(f(a), o)(_ :: _))
        // as match {
        //     case Nil => Some(Nil)
        //     case x :: xs => f(x) flatMap (xx => traverse(xs)(f) map (xx :: _))
        // }

    def sequenceViTraverse[A](os: List[MyOption[A]]): MyOption[List[A]] = traverse(os)(a => a)

    def Try[A](a: => A): MyOption[A] = 
        try Some(a)
        catch {
            case _: Exception => None
        }
}

object Insurance {
    import MyOption._

    def insuranceRateQuote(age: Int, numTickets: Int): Double = {
        5.0;
    }

    def parseInuranceRateQuote(age: String, numTickets: String): MyOption[Double] = {
        val optAge: MyOption[Int] = Try(age.toInt)
        val optTix: MyOption[Int] = Try(numTickets.toInt)
        
        map2(optAge, optTix)(insuranceRateQuote _) 
    }
}

object Main {
    import MyOption._

    def mean(ys: Seq[Double]): MyOption[Double] = ys.length match {
        case 0 => None
        case _ => Some(ys.sum / ys.length)
    }
    
       //  try {
       //      Some(ys.sum / ys.length)
       //  } catch {
       //      case _: Throwable => None 
       //  }

    def variance(xs: Seq[Double]): MyOption[Double] = {
        mean(xs).map(m => xs.map(x => math.pow(x - m, 2))).flatMap(mean _) 
    }

    def main(args: Array[String]) {
        val xs = List(1.0, 2.0, 3.0, 4.0)
        println("var: " + variance(xs))
        println("traverse: " + traverse(xs)(x => Try(x.toString)))
    }
}

