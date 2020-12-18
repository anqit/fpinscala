package Chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine => {
        inputs match {
            case Nil => ((machine.coins, machine.candies), machine)
            case i :: is => simulateMachine(is).run(transition(machine, i))
        }
    })

    def transition(m: Machine, i: Input): Machine = i match {
            case Coin if m.candies > 0 && m.locked => m.copy(locked = false, coins = m.coins + 1)
            case Turn if m.candies > 0 && !m.locked =>m.copy(locked = true, candies = m.candies - 1)
            case _ => m
        }
}

object Main {
    def main(args: Array[String]): Unit = {
        val m = Machine(true, coins = 10, candies = 5)
        val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
        val sm = Machine.simulateMachine(inputs).run(m)

        println(sm)
    }
}