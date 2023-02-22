package util

object Utils:
    def b(): Unit = p("--------")
    def p(a: Any): Unit = println(a)
    
    def time[A](f: => A): (Long, A) =
        val now = System.nanoTime()
        val ret = f
        val elapsed = System.nanoTime() - now
        (elapsed, ret)