package chapter12

import chapter10.{Monoid, Semigroup}
import chapter11.Functor
import chapter4.Validated
import chapter6.State

type Id = [x] =>> x
type Product = [a[_], b[_]] =>> [x] =>> (a[x], b[x])

trait Applicative[F[_]] extends Functor[F]:
    self =>
    def unit[A](a: => A): F[A]

    def apply[A, B](fab: F[A => B], fa: F[A]): F[B] =
        fab.map2(fa) { _(_) }

    def sequence[A](fas: List[F[A]]): F[List[A]] =
//        fas.foldRight(unit[List[A]](Nil)) { (fa, as) => fa.map2(as) { _ :: _ } }
        fas.foldRight(unit[List[A]](Nil)) { _.map2(_) { _ :: _ } }

    def sequence[K, V](ofv: Map[K, F[V]]): F[Map[K, V]] =
        ofv.foldRight(unit[Map[K, V]](Map.empty)) {
            case ((k, fv), fkvs) => fkvs.map2(fv) { (kvs, v) => kvs.updated(k, v) }
        }

    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
        as.foldRight(unit[List[B]](Nil)) { (a, bs) => f(a).map2(bs) { _ :: _ } }

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
        sequence(List.fill(n)(fa))

    def compose[G[_]](G: Applicative[G]): Applicative[[x] =>> F[G[x]]] = new:
        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))

        override def apply[A, B](fgab: F[G[A => B]], fga: F[G[A]]): F[G[B]] =
            self.map2(fgab)(fga) { G.apply }

    def product[G[_]](G: Applicative[G]): Applicative[Product[F, G]] = new:
        private type PFG = [x] =>> Product[F, G][x]

        override def unit[A](a: => A): PFG[A] = (self.unit(a), G.unit(a))

        override def apply[A, B](fab: PFG[A => B], fa: PFG[A]): PFG[B] =
            (self.apply(fab(0), fa(0)), G.apply(fab(1), fa(1)))

    extension [A](fa: F[A])
        def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
            apply[B, C](apply[A, B => C](unit(a => f.curried(a)), fa), fb)

        def map3[B, C, D](fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
            apply(apply(apply(unit(a => f.curried(a)), fa), fb), fc)

        def map4[B, C, D, E](fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
            apply(apply(apply(apply(unit(a => f.curried(a)), fa), fb), fc), fd)

        def map[B](f: A => B): F[B] =
            apply(unit(f), fa)

        def map_viaMap2[B](f: A => B): F[B] =
            fa.map2(unit(())) { (a, _) => f(a) }

        def product[B](fb: F[B]): F[(A, B)] =
            fa.map2(fb) { (_, _) }

object Applicative:
    given idApplicative: Applicative[Id] with
        override def unit[A](a: => A): Id[A] = a

        extension [A](fa: Id[A])
            override def map2[B, C](fb: Id[B])(f: (A, B) => C): Id[C] =
                f(fa, fb)

    given validatedApplicative[E: Semigroup]: Applicative[[x] =>> Validated[E, x]] with
        override def unit[A](a: => A): Validated[E, A] = Validated.Valid(a)

        extension [A](fa: Validated[E, A])
            override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
                fa.map2(fb)(f)(summon[Semigroup[E]].combine)

opaque type ZipList[+A] = LazyList[A]
object ZipList:
    def fromLazyList[A](la: LazyList[A]): ZipList[A] = la
    extension [A](za: ZipList[A]) def toLazyList: LazyList[A] = za

    given zipListApplicative: Applicative[ZipList] with
        def unit[A](a: => A): ZipList[A] =
            LazyList.continually(a)

        extension [A](fa: ZipList[A])
            override def map2[B, C](fb: ZipList[B])(f: (A, B) => C): ZipList[C] =
                fa.zip(fb).map(f.tupled)

trait Traverse[F[_]] extends Functor[F]:
    self =>
    import chapter11.Monad.given

    def compose[G[_]: Traverse]: Traverse[[x] =>> F[G[x]]] = new:
        extension [A](fa: F[G[A]])
            override def traverse[H[_]: Applicative, B](f: A => H[B]): H[F[G[B]]] =
                self.traverse(fa)(ga => ga.traverse(f))

    extension [A](fa: F[A])
        def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] =
            fa.map(f).sequence

        override def map[B](f: A => B): F[B] =
            traverse[Id, B](f)

        def mapAccum[S, B](s: S)(f: (A, S) => (B, S)): (F[B], S) =
            fa.traverse[[x] =>> State[S, x], B](a =>
                for
                    s1 <- State.get[S]
                    (b, s2) = f(a, s1)
                    _ <- State.set(s2)
                yield b
            ).apply(s)

        def toList: List[A] =
            fa.mapAccum(List[A]())((a, s) => ((), a :: s))(1).reverse

        def reverse: F[A] =
            fa.mapAccum(fa.toList.reverse)((a, s) => (s.head, s.tail))(0)

        def zipWithIndex: F[(A, Int)] =
            fa.mapAccum(0)((a, s) => ((a, s), s + 1))(0)

        def foldLeft[B](b: B)(f: (B, A) => B): B =
            mapAccum(b)((a, s) => ((), f(s, a)))(1)

        def fuse[M[_] : Applicative, N[_] : Applicative, B](
            f: A => M[B], g: A => N[B]
        ): (M[F[B]], N[F[B]]) =
            fa.traverse[Product[M, N], B] { a =>
                (f(a), g(a))
            }(using summon[Applicative[M]].product(summon[Applicative[N]]))
//            (fa.map(f).sequence, fa.map(g).sequence)

    extension [G[_]: Applicative, A](fga: F[G[A]])
        def sequence: G[F[A]] =
            fga.traverse(ga => ga)

object Traverse:
    given listTraverse: Traverse[List] with
        extension [A](fa: List[A])
            override def map[B](f: A => B): List[B] = fa.map(f)

            override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
                fa.map(f).foldRight(summon[Applicative[G]].unit(List[B]())) { _.map2(_) { _ :: _ } }

    given optionTraverse: Traverse[Option] with
        extension [A](fa: Option[A])
            override def map[B](f: A => B): Option[B] = fa.map(f)

            override def traverse[G[_] : Applicative, B](f: A => G[B]): G[Option[B]] =
                fa.map(f) match
                    case Some(gb) => gb.map(Some(_))
                    case None => summon[Applicative[G]].unit(None)

    case class Tree[+A](head: A, tail: List[Tree[A]])

    given treeTraverse: Traverse[Tree] with
        extension [A](fa: Tree[A])
            override def map[B](f: A => B): Tree[B] =
                fa match
                    case Tree(h, ts) => Tree(f(h), ts.map(_.map(f)))

            override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] =
                fa.map(f) match
                    case Tree(gb, ltgb) =>
                        val gltb = ltgb.map(t => t.sequence).sequence
                        gb.map2(gltb) { Tree(_, _) }

    given mapTraverse[K]: Traverse[[x] =>> Map[K, x]] with
        extension [A](fa: Map[K, A])
            override def map[B](f: A => B): Map[K, B] =
                fa.view.mapValues(f).toMap

            override def traverse[G[_]: Applicative, B](f: A => G[B]): G[Map[K, B]] =
                fa.view.mapValues(f).foldRight(summon[Applicative[G]].unit(Map[K, B]())) {
                    case ((k, gb), gkbs) => gb.map2(gkbs) { (b, kbs) => kbs.updated(k, b) }
                }

