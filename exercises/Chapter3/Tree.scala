sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeRunner {
    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }
 
    def maximum(tree: Tree[Int]): Int = tree match {
        case Leaf(x) => x
        case Branch(l, r) => maximum(l) max maximum(r)
    }
 
    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
 
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(a) => Leaf(f(a))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A,B](tree: Tree[A])(l: A => B)(b: (B,B) => B): B = tree match {
        case Leaf(a) => l(a)
        case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
    }

    def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)
    def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(identity[Int])(_ max _)
    def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + (l max r))
    def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
 
    def main(args: Array[String]) = {
        val treeA = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)));
        println("tree: " + treeA)
        println("size: " + size(treeA))
        println("max: " + maximum(treeA))
        println("depth: " + depth(treeA))
        println("map: " + map(treeA)(toWord(_)))
        println("size via fold: " + sizeViaFold(treeA))
        println("max via fold: " + maximumViaFold(treeA))
        println("depth via fold: " + depthViaFold(treeA))
        println("map via fold: " + mapViaFold(treeA)(a => toWord(a * 10)))
        println("sum: " + fold(treeA)(v => v)(_ + _)) 
    }
 
    private def toWord(i: Int): String = i match {
        case 1 => "one"
        case 2 => "two"
        case 3 => "three"
        case 4 => "four"
        case 10 => "ten"
        case 20 => "twenty"
        case 30 => "thirty"
        case 40 => "forty"
        case _ => "can't count that high"
    }
}
