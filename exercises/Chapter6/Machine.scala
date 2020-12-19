package Chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def update(i: Input) = i match {
        case Coin if candies > 0 && locked => Machine(false, candies, coins + 1)
        case Turn if candies > 0 && !locked => Machine(true, candies - 1, coins)
        case _ => this
    }
}

object Machine {
    import State._

    def transition(m: Machine, i: Input): Machine = i match {
            case Coin if m.candies > 0 && m.locked => m.copy(locked = false, coins = m.coins + 1)
            case Turn if m.candies > 0 && !m.locked => m.copy(locked = true, candies = m.candies - 1)
            case _ => m
        }

    // I guess I completely missed the point here...
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine =>
        inputs match {
            case Nil => ((machine.coins, machine.candies), machine)
            case i :: is => simulateMachine(is).run(machine.update(i))
        })

    def update = (i: Input) => (s: Machine) =>
        (i, s) match {
            case (_, Machine(_, 0, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(true, candy, coin)) =>
                Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
                Machine(true, candy - 1, coin)
        }

    def simulateMachine_book(inputs: List[Input]): State[Machine, (Int, Int)] = for {
            _ <- sequence(inputs map (modify[Machine] _ compose update))
            s <- get
        } yield (s.coins, s.candies)

    // the IMPORTANT thing to remember/realize here is that each of the flatMap/map/sequence.. etc
    // functions are internally `update()`ing and propagating that updated state (Machine)
    // to the next combinator. thus, at the end, the given machine will have processed
    // all the given `inputs`, but as the state of the State, not the action. only at the end,
    // is the state (Machine) returned as the action as well (via `get()`) and mapped
    // to the desired output tuple
    // this is an interesting use of State, where the action pretty much is irrelevant
    // throughout the processing, until we `get()` it at the end
    def simulateMachine_translated(inputs: List[Input]): State[Machine, (Int, Int)] = {
        // a function that takes a function that transforms a machine, and returns
        // a State whose run() method returns the transformed machine as the next state
        // "tell me how to modify a machine"
        val modMach: (Machine => Machine) => State[Machine, Unit] = modify[Machine] _

        // a function that takes an input and returns a State whose run() method
        // returns the machine updated by the input as the next state
        // "if you give me an input, i can modify a machine"
        val comp: Input => State[Machine, Unit] = modMach compose update

        // maps all of the inputs to `State`s whose run() method returns the given machine
        // updated by the input as the next state
        // "if you give me a machine, i will perform these modifications in this order on that machine"
        val mapped: List[State[Machine, Unit]] = inputs map comp

        // returns a State whose Action is just an accumulation of Units, but whose
        // state (a Machine) has been updated by subsequent transformations traversing the
        // `mapped` List
        // "i have updated the machine as instructed, and now hold that updated machine in my internal state"
        val seq: State[Machine, List[Unit]] = sequence(mapped)

        // flatMaps `seq`, ignores the action (List[Unit]), returns a `get`
        // that maps the `seq` state (the transformed Machine) to the desired tuple action
        // "i will expose my internal state as an action and transform it to the desired structure"
        val ret = seq.flatMap(_ => get.map(s => (s.coins, s.candies)))

        ret
    }
}

object Main {
    def main(args: Array[String]): Unit = {
        val m = Machine(true, coins = 10, candies = 5)
        val inputs = List(Coin, Turn, Turn, Turn, Coin, Coin, Coin, Turn, Coin, Turn, Turn, Coin, Turn)
        val sm = Machine.simulateMachine(inputs).run(m)

        println(sm)
    }
}
