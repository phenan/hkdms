package com.phenan.hkdms.hkd

import cats.Applicative
import com.phenan.hkdms.syntax.*
import com.phenan.hkdms.util.*

import scala.deriving.Mirror

sealed trait HKTree [R, F[_]] {
  def map [G[_]](f: [t] => F[t] => G[t]): HKTree[R, G]

  def fold (using applicative: Applicative[F]): F[R]
}

case class HKProduct [R <: Product, F[_]] (hkd: HKD[R, [e] =>> HKTree[e, F]])(using mirror: Mirror.ProductOf[R]) extends HKTree[R, F] {
  def map [G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G] = {
    HKProduct(hkd.map([u] => (hkt: HKTree[u, F]) => hkt.map(f)))
  }
  def fold (using applicative: Applicative[F]): F[R] = {
    applicative.map(applicative.productAll(hkd.map[F]([u] => (hkt: HKTree[u, F]) => hkt.fold).asTuple))(mirror.fromProduct)
  }
}
case class HKMapped [T, R, F[_]] (tree: HKTree[T, F], mapper: T => R) extends HKTree[R, F] {
  def map [G[_]](f: [t] => F[t] => G[t]): HKMapped[T, R, G] = {
    HKMapped(tree.map(f), mapper)
  }
  def fold (using applicative: Applicative[F]): F[R] = {
    applicative.map(tree.fold)(mapper)
  }
}
trait HKSum [R, F[_]] extends HKTree[R, F] {
  def map [G[_]](f: [t] => F[t] => G[t]): HKSum[R, G]
}
case class HKValue [R, F[_]] (value: F[R]) extends HKTree[R, F] {
  def map [G[_]](f: [t] => F[t] => G[t]): HKValue[R, G] = HKValue(f[R](value))
  def fold (using applicative: Applicative[F]): F[R] = value
}
