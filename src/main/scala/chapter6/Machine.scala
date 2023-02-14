package chapter6

import util.Utils.*

enum Input:
    case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine:
    import Input.*
    import State.*

    extension (m: Machine)
        def apply(i: Input): Machine =
            transition(m, i)

        def hasCandies = m.candies > 0

        def sim(inputs: List[Input]): ((Int, Int), Machine) =
            simulate(inputs)(m)

    def transition(m: Machine, i: Input): Machine =
        i match
            case Coin if m.locked && m.hasCandies => m.copy(locked = false, coins = m.coins + 1)
            case Turn if !m.locked && m.hasCandies => m.copy(locked = true, candies = m.candies - 1)
            case _ => m

    def simulate(inputs: List[Input]): State[Machine, (Int, Int)] =
        for
            _ <- sequence(inputs map { i => State.modify[Machine] { _(i) } })
            m <- get
        yield (m.coins, m.candies)

    @main
    def machines(): Unit =
        val m = Machine(true, candies = 5, coins = 10)
        val inputs = List(Turn, Coin, Turn, Coin, Coin, Turn, Turn, Coin, Turn, Coin, Turn)
        p(m sim inputs)
