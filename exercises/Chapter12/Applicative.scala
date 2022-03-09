package Chapter12

import Chapter10.{ Foldable, Monoid }
import Chapter11.{ Functor, Id }

import scala.language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

    def map[A, B](fa: F[A])(f: A => B): F[B] =
        map2(fa, unit(())) { (a, _) => f(a) }

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
        as.foldRight(unit(List[B]())) { (a, bs)  => map2(f(a), bs) { _ :: _ } }

    def sequence[A](as: List[F[A]]): F[List[A]] =
        traverse(as) { fa => fa }

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
        sequence(List.fill(n)(fa))

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
        map2(fa, fb) { (_, _) }

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
        map2(fab, fa)(_(_))

    def map_viaApply[A, B](fa: F[A])(f: A => B): F[B] =
        apply(unit(f))(fa)

    def map2_viaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
        apply(apply(unit[A => B => C](a => f(a, _)))(fa))(fb)

    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
        apply(
            apply(
                apply(unit(f.curried))(fa)
            )(fb)
        )(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
        apply(
            apply(
                apply(
                    apply(unit(f.curried))(fa)
                )(fb)
            )(fc)
        )(fd)

    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
        val self = this
        new Applicative[({type f[x] = (F[x], G[x])})#f] {
            def unit[A](a: => A): (F[A], G[A]) =
                (self.unit(a), G.unit(a))

            def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) =
                (self.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
        }
    }

    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
        val self = this
        new Applicative[({type f[x] = F[G[x]]})#f] {
            override def unit[A](a: => A): F[G[A]] =
                self.unit(G.unit(a))

            override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
                self.map2(fa, fb) { G.map2(_, _)(f) }
        }
    }

    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
        ofa.foldLeft(unit(Map[K, V]())) { case (fm, (k, fv)) =>
            map2(fm, fv) { (m, v) => m + (k -> v) }
        }
}

trait Traverse[F[_]] extends Applicative[F] with Foldable[F] {
    import Traverse._

    def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
        sequence(map(fa)(f))

    def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
        traverse(fga)(ga => ga)

    override def map[A, B](fa: F[A])(f: A => B): F[B] =
        traverse(fa)(a => Id(f(a)))(Id.idMonad[A]).value

    override def foldMap[A, M](as: F[A])(f: A => M)(m: Monoid[M]): M =
        traverse[({ type f[x] = Const[M, x] })#f, A, Nothing](as)(f)(monoidApplicative(m))
}

object Traverse {
    type Const[M, B] = M

    implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({type f[x] = Const[M, x]})#f] =
        new Applicative[({type f[x] = Const[M, x]})#f] {
            def unit[A](a: => A): M = M.zero
            def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = M.op(m1, m2)
        }

    val listTraverse: Traverse[List] = new Traverse[List] {
        override def unit[A](a: => A): List[A] = List(a)

        override def map2[A, B, C](fa: List[A], fb: List[B])(f: (A, B) => C): List[C] =
            fa zip fb map f.tupled

        override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
            val appG = implicitly[Applicative[G]]
            fa.foldRight(appG.unit(List[B]())) { (a, gl) =>
                appG.map2(f(a), gl)(_ :: _)
            }
        }

        override def sequence[G[_] : Applicative, A](fga: List[G[A]]): G[List[A]] = {
            val appG = implicitly[Applicative[G]]
            fga.foldRight(appG.unit(List.empty[A])) { (ga, gl) =>
                appG.map2(ga, gl)(_ :: _)
            }
        }
    }

    val optionTraverse: Traverse[Option] = new Traverse[Option] {
        override def unit[A](a: => A): Option[A] = Option(a)

        override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
            for {
                a <- fa
                b <- fb
            } yield f(a, b)

        override def traverse[G[_] : Applicative, A, B](oa: Option[A])(f: A => G[B]): G[Option[B]] = {
            val appG = implicitly[Applicative[G]]
            oa.foldRight(appG.unit(Option.empty[B])) { (a, _) =>
                appG.map(f(a)) { Option(_) }
            }

            oa match {
                case Some(a) => appG.map(f(a)) { Option(_) }
                case None => appG.unit(None)
            }
        }

        override def sequence[G[_] : Applicative, A](oga: Option[G[A]]): G[Option[A]] = {
            val appG = implicitly[Applicative[G]]
            oga.foldRight(appG.unit(Option.empty[A])) { (ga, _) =>
                appG.map(ga) { Option(_) }
            }

            oga match {
                case Some(ga) => appG.map(ga) { Option(_) }
                case None => appG.unit(Option.empty[A])
            }
        }
    }

    val treeTraverse: Traverse[Tree] = new Traverse[Tree] {
        override def unit[A](a: => A): Tree[A] = Tree.leaf(a)
        override def map2[A, B, C](ta: Tree[A], tb: Tree[B])(f: (A, B) => C): Tree[C] = (ta, tb) match {
            case (Tree(ah, at), Tree(bh, bt)) => Tree(f(ah, bh), _map2(at, bt)(f))
        }

        def _map2[A, B, C](lta: List[Tree[A]], ltb: List[Tree[B]])(f: (A, B) => C): List[Tree[C]] = (lta, ltb) match {
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (ta :: tas, tb :: tbs) => map2(ta, tb)(f) :: _map2(tas, tbs)(f)
        }

        override def sequence[G[_] : Applicative, A](tga: Tree[G[A]]): G[Tree[A]] = {
            val appG = implicitly[Applicative[G]]

            tga match {
                case Tree(ga, ltga) =>
                    val lgta: List[G[Tree[A]]] = ltga map { tga => sequence(tga) }
                    val glta: G[List[Tree[A]]] = listTraverse.sequence(lgta)
                    appG.map2(ga, glta) { Tree(_, _) }
            }
        }

    }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Tree {
    def leaf[A](a: A): Tree[A] = Tree(a, Nil)
}
