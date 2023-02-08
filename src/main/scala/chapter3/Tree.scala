package chapter3

import util.Utils.p

enum Tree[+A]:
    case Leaf(value: A)
    case Branch(left: Tree[A], right: Tree[A])

    def fold[B](fl: A => B, fb: (B, B) => B): B = this match
        case Leaf(l) => fl(l)
        case Branch(l, r) => fb(l.fold(fl, fb), r.fold(fl, fb))

    def fold[B](b: B, fb: (B, B) => B): B = this match
        case Leaf(l) => b
        case Branch(l, r) => fb(l.fold(b, fb), r.fold(b, fb))

    def size: Int = fold(1, (l, r) => 1 + l + r)

    def depth: Int = fold(1, (l, r) => 1 + (l max r))

    def map[B](f: A => B): Tree[B] = fold(l => Leaf(f(l)), (l, r) => Branch(l, r))

object Tree:
    extension [A](t: Tree[A])(using num: Numeric[A])
        def firstPositive: A = t match
            case Leaf(i) => i
            case Branch(l, r) =>
                val lpos = l.firstPositive
                if num.gt(lpos, num.zero) then lpos else r.firstPositive

        def maximum: A = t.fold(l => l, num.max(_, _))

    @main
    def trees(): Unit =
        val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(10), Leaf(4)), Leaf(5)))
        p(tree map { _ * 10 })
        p(tree.size)
        p(tree.depth)
        p(tree.maximum)