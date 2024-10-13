package util

object Utils:
    def b(): Unit = p("--------")
    def p(a: Any): Unit = println(a)

    def time[A](f: => A, log: Boolean = false): (Long, A) =
        val start = System.nanoTime()
        if log then p(start)

        val ret = f

        val end = System.nanoTime()
        val elapsed = end - start
        if log then
            p(end)
            p(elapsed)

        (elapsed, ret)
