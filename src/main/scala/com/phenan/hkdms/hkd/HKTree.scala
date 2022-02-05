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
  private class ProductHKTreeImpl [R <: Product, F[_]] (hkd: HKD[R, F], mirror: Mirror.ProductOf[R]) extends HKProduct[R, F] {
    def map [G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G] = new ProductHKTreeImpl(hkd.map(f), mirror)
    def fold (using applicative: Applicative[F]): F[R] = {
      applicative.map(applicative.productAll(hkd.asTuple(using mirror)))(mirror.fromProduct)
    }
  }
  private class SumHKTreeImpl [R, F[_], T <: R] (value: HKTree[T, F]) extends HKSum[R, F] {
    def map [G[_]](f: [t] => F[t] => G[t]): HKSum[R, G] = {
      new SumHKTreeImpl[R, G, T](value.map(f))
    }
    def fold (using applicative: Applicative[F]): F[R] = {
      applicative.widen(value.fold)
    }
  }
  private class ValueHKTreeImpl [R, F[_]] (val value: F[R]) extends HKValue[R, F] {
    def map [G[_]](f: [t] => F[t] => G[t]): HKValue[R, G] = {
      new ValueHKTreeImpl(f[R](value))
    }
    def fold (using applicative: Applicative[F]): F[R] = value
  }
}
