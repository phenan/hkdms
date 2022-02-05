package com.phenan.hkdms.hkd

import cats.Id

import com.phenan.hkdms.util.*

import scala.deriving.Mirror
import scala.language.dynamics

trait HKD [R <: Product, F[_]] extends Dynamic {
  def selectDynamic[Tag <: Singleton](tag: Tag)(using mirror: Mirror.ProductOf[R], index: ValueOf[Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]): F[Tuple.Elem[mirror.MirroredElemTypes, Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]]

  def map [G[_]](f: [t] => F[t] => G[t]): HKD[R, G]

  def asTuple (using mirror: Mirror.ProductOf[R]): Tuple.Map[mirror.MirroredElemTypes, F]
}

private class HKDImpl [R <: Product, F[_], T <: Tuple] (tuple: Tuple.Map[T, F]) extends HKD[R, F] {
  def selectDynamic[Tag <: Singleton](tag: Tag)(using mirror: Mirror.ProductOf[R], index: ValueOf[Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]): F[Tuple.Elem[mirror.MirroredElemTypes, Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]] = {
    tuple.productElement(index.value).asInstanceOf[F[Tuple.Elem[mirror.MirroredElemTypes, Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]]]
  }
  def map [G[_]](f: [t] => F[t] => G[t]): HKD[R, G] = {
    new HKDImpl[R, G, T](TupleMaps.map(tuple)(f))
  }
  def asTuple (using mirror: Mirror.ProductOf[R]): Tuple.Map[mirror.MirroredElemTypes, F] = {
    tuple.asInstanceOf[Tuple.Map[mirror.MirroredElemTypes, F]]
  }
}

object HKDOf extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(args: Tuple.Map[mirror.MirroredElemTypes, F]): HKD[R, F] = new HKDImpl(args)
  def applyDynamicNamed[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(params: Tuple.Zip[mirror.MirroredElemLabels, Tuple.Map[mirror.MirroredElemTypes, F]]): HKD[R, F] = {
    val args = for (i <- 0 until params.size) yield {
      params.productElement(i).asInstanceOf[(_, _)]._2
    }
    new HKDImpl(Tuple.fromArray(args.toArray).asInstanceOf[Tuple.Map[mirror.MirroredElemTypes, F]])
  }
}
