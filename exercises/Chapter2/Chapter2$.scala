object Chapter2 {
    def fib(n: Int): Int = {
        @annotation.tailrec
        def go(n: Int, prev: Int, curr: Int): Int = {
            if(n == 0) prev
            else go(n - 1, curr, prev + curr)
        }

        go(n, 0, 1)
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def go(n: Int): Boolean = {
            if(n >= as.length - 1) true
            else if(!ordered(as(n), as(n + 1))) false
            else go(n + 1)
        }

        go(0)
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = f.curried // f(a, _) // a => ((b: B) => f(a, b))

    def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

    def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

    def main(args: Array[String]): Unit = {
        // println(fib(5));
        println(isSorted(Array(0, 1, 2, 3, 2), (a: Int, b: Int) => a < b))
    }
}
