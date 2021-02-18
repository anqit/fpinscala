package Chapter8

trait Gen[A]

object Gen {
    def listOf[A](a: Gen[A]): Gen[List[A]] = ???

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}

trait Prop {
    def check: Boolean

    def &&(p: Prop): Prop = Prop(() => check && p.check)
}

object Prop {
    def apply(check: () => Boolean): Prop = new Prop {
        override def check: Boolean = check
    }
}