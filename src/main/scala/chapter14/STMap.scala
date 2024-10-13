package chapter14

import scala.collection.mutable

final class STMap[S, K, V] private (private var value: mutable.Map[K, V]):
    def size: ST[S, Int] = ST(value.size)
    
    def write(k: K, v: V): ST[S, Unit] =
        ST.lift[S, Unit]:
            s =>
                value(k) = v
                ((), s)
    
    def read(k: K): ST[S, V] =
        ST(value(k))
        
    def freeze: ST[S, Map[K, V]] = ST(Map.from(value))