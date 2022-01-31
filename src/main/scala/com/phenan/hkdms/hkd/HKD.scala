package com.phenan.hkdms.hkd

import cats.Id

import com.phenan.hkdms.util.*

import scala.deriving.Mirror
import scala.language.dynamics

trait HKD [R <: Product, F[_]] extends Dynamic {
  def selectDynamic[Tag <: Singleton](tag: Tag)(using mirror: Mirror.ProductOf[R], index: ValueOf[Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]): F[Tuple.Elem[mirror.MirroredElemTypes, Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]]

  def map [G[_]](f: [t] => F[t] => G[t]): HKD[R, G]
}

object HKD {
  def fromProduct[R <: Product](value: R)(using mirror: Mirror.ProductOf[R]): HKD[R, Id] = {
    new HKDImpl(Tuple.fromProductTyped(value).map[Id]([t] => (v: t) => v))
  }

  def fromTuple[R <: Product, F[_]](using mirror: Mirror.ProductOf[R])(tuple: Tuple.Map[mirror.MirroredElemTypes, F]): HKD[R, F] = {
    new HKDImpl(tuple)
  }

  private class HKDImpl [R <: Product, F[_], T <: Tuple] (tuple: Tuple.Map[T, F]) extends HKD[R, F] {
    def selectDynamic[Tag <: Singleton](tag: Tag)(using mirror: Mirror.ProductOf[R], index: ValueOf[Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]): F[Tuple.Elem[mirror.MirroredElemTypes, Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]] = {
      tuple.productElement(index.value).asInstanceOf[F[Tuple.Elem[mirror.MirroredElemTypes, Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]]]
    }
    def map [G[_]](f: [t] => F[t] => G[t]): HKD[R, G] = {
      new HKDImpl[R, G, T](TupleMaps.map(tuple)(f))
    }
  }
}
