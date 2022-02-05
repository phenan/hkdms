package com.phenan.hkdms.hkd

import cats.Applicative
import com.phenan.hkdms.syntax.*
import com.phenan.hkdms.util.*

import scala.deriving.Mirror
import scala.language.dynamics

sealed trait HKTree [R, F[_]] {
  def map [U] (f: R => U) : HKTree[U, F] = new HKMapped(this, f)

  def hmap [G[_]](f: [t] => F[t] => G[t]): HKTree[R, G]

  def widen [U >: R] : HKTree[U, F] = map(identity)

  def fold (using applicative: Applicative[F]): F[R]

  def foldMap [G[_]](compiler: [t] => F[t] => G[t])(using applicative: Applicative[G]): G[R] = hmap(compiler).fold
}

trait HKProduct [R <: Product, F[_]] extends HKTree[R, F] {
  def hmap[G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G]
}
private class HKProductImpl [R <: Product, F[_]] (hkd: HKD[R, [e] =>> HKTree[e, F]])(using mirror: Mirror.ProductOf[R]) extends HKProduct[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G] = {
    new HKProductImpl(hkd.map([u] => (hkt: HKTree[u, F]) => hkt.hmap(f)))
  }
  def fold (using applicative: Applicative[F]): F[R] = {
    applicative.map(applicative.productAll(hkd.map[F]([u] => (hkt: HKTree[u, F]) => hkt.fold).asTuple))(mirror.fromProduct)
  }
}
class HKMapped [T, R, F[_]] (tree: HKTree[T, F], mapper: T => R) extends HKTree[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKMapped[T, R, G] = {
    new HKMapped(tree.hmap(f), mapper)
  }
  def fold (using applicative: Applicative[F]): F[R] = {
    applicative.map(tree.fold)(mapper)
  }
}
case class HKValue [R, F[_]] (value: F[R]) extends HKTree[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKValue[R, G] = HKValue(f[R](value))
  def fold (using applicative: Applicative[F]): F[R] = value
}

object HKProduct extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(args: Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKTree[e, F]]): HKProduct[R, F] = new HKProductImpl(HKD(args))
  def applyDynamicNamed[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(params: Tuple.Zip[mirror.MirroredElemLabels, Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKTree[e, F]]]): HKProduct[R, F] = {
    val args = for (i <- 0 until params.size) yield {
      params.productElement(i).asInstanceOf[(_, _)]._2
    }
    new HKProductImpl(HKD(Tuple.fromArray(args.toArray).asInstanceOf[Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKTree[e, F]]]))
  }
}
