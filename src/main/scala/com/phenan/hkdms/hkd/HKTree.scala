package com.phenan.hkdms.hkd

import cats.Applicative
import com.phenan.hkdms.syntax.*
import com.phenan.hkdms.util.*

import scala.deriving.Mirror

sealed trait HKTree [R, F[_]] {
  def map [G[_]](f: [t] => F[t] => G[t]): HKTree[R, G]

  def fold (using applicative: Applicative[F]): F[R]
}

trait HKProduct [R <: Product, F[_]] extends HKTree[R, F] {
  def map [G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G]
}
trait HKSum [R, F[_]] extends HKTree[R, F] {
  def map [G[_]](f: [t] => F[t] => G[t]): HKSum[R, G]
}
trait HKValue [R, F[_]] extends HKTree[R, F] {
  def value: F[R]
  def map [G[_]](f: [t] => F[t] => G[t]): HKValue[R, G]
}

object HKTree {
  private class HKProductImpl [R <: Product, F[_]] (hkd: HKD[R, [e] =>> HKTree[e, F]], mirror: Mirror.ProductOf[R]) extends HKProduct[R, F] {
    def map [G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G] = {
      new HKProductImpl(hkd.map([u] => (hkt: HKTree[u, F]) => hkt.map(f)), mirror)
    }
    def fold (using applicative: Applicative[F]): F[R] = {
      applicative.map(applicative.productAll(hkd.map[F]([u] => (hkt: HKTree[u, F]) => hkt.fold).asTuple(using mirror)))(mirror.fromProduct)
    }
  }
  private class HKSumImpl [R, F[_], T <: R] (value: HKTree[T, F]) extends HKSum[R, F] {
    def map [G[_]](f: [t] => F[t] => G[t]): HKSum[R, G] = {
      new HKSumImpl[R, G, T](value.map(f))
    }
    def fold (using applicative: Applicative[F]): F[R] = {
      applicative.widen(value.fold)
    }
  }
  private class HKValueImpl [R, F[_]] (val value: F[R]) extends HKValue[R, F] {
    def map [G[_]](f: [t] => F[t] => G[t]): HKValue[R, G] = {
      new HKValueImpl(f[R](value))
    }
    def fold (using applicative: Applicative[F]): F[R] = value
  }
}
