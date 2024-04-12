package chapter10

trait Semigroup[A]:
  def combine(a1: A, a2: A): A

trait Monoid[A] extends Semigroup[A]:
  def identity(a: => A): A
