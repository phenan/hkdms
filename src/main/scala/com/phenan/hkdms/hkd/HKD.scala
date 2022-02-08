package com.phenan.hkdms.hkd

import cats.{Id, InvariantMonoidal}
import com.phenan.hkdms.syntax.*
import com.phenan.hkdms.util.*

import scala.deriving.Mirror
import scala.language.dynamics

trait HKD [R <: Product, F[_]] extends Dynamic {
  def selectDynamic[Tag <: Singleton](tag: Tag)(using mirror: Mirror.ProductOf[R], index: ValueOf[Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]): F[Tuple.Elem[mirror.MirroredElemTypes, Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]]

  def map [G[_]](f: [t] => F[t] => G[t]): HKD[R, G]

  def fold (using mirror: Mirror.ProductOf[R], invariantMonoidal: InvariantMonoidal[F]): F[R]

  def foldMap [G[_]] (f: [t] => F[t] => G[t])(using mirror: Mirror.ProductOf[R], invariantMonoidal: InvariantMonoidal[G]): G[R] = map(f).fold
}

private class HKDImpl [R <: Product, F[_], T <: Tuple] (tuple: Tuple.Map[T, F]) extends HKD[R, F] {
  def selectDynamic[Tag <: Singleton](tag: Tag)(using mirror: Mirror.ProductOf[R], index: ValueOf[Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]): F[Tuple.Elem[mirror.MirroredElemTypes, Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]] = {
    tuple.productElement(index.value).asInstanceOf[F[Tuple.Elem[mirror.MirroredElemTypes, Tuples.IndexOf[mirror.MirroredElemLabels, Tag]]]]
  }
  def map [G[_]](f: [t] => F[t] => G[t]): HKD[R, G] = {
    new HKDImpl[R, G, T](TupleMaps.map(tuple)(f))
  }
  def fold (using mirror: Mirror.ProductOf[R], invariantMonoidal: InvariantMonoidal[F]): F[R] = {
    val product: F[mirror.MirroredElemTypes] = invariantMonoidal.productAll(tuple.asInstanceOf[Tuple.Map[mirror.MirroredElemTypes, F]])
    invariantMonoidal.imap(product)(mirror.fromProduct)(Tuple.fromProductTyped(_))
  }
}

object HKD extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(args: Tuple.Map[mirror.MirroredElemTypes, F]): HKD[R, F] = new HKDImpl(args)
  def applyDynamicNamed[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(params: Tuple.Zip[mirror.MirroredElemLabels, Tuple.Map[mirror.MirroredElemTypes, F]]): HKD[R, F] = {
    val args = for (i <- 0 until params.size) yield {
      params.productElement(i).asInstanceOf[(_, _)]._2
    }
    new HKDImpl(Tuple.fromArray(args.toArray).asInstanceOf[Tuple.Map[mirror.MirroredElemTypes, F]])
  }
}

given [T, F[_]] : Conversion[F[T], Tuple.Map[T *: EmptyTuple, F]] = {
  _ *: EmptyTuple
}
