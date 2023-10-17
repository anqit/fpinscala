package chapter8

import Gen.*

object TestRunner:
    import Gen.*
    import Prop.*
    
    def testMaxOfIntList: Prop =
        val smallInt = Gen.choose(-10, 10)
        val maxProp = Prop.forAll(smallInt.list) { l =>
            val max = l.max
            l.forall { _ <= max }
        }
        maxProp
    
    @main
    def tryTests(): Unit =
        val prop = testMaxOfIntList
        prop.run()