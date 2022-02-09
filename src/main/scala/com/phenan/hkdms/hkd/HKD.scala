package com.phenan.hkdms.hkd

import cats.InvariantMonoidal
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

type HKDElems[Types <: Tuple, F[_]] = Types match {
  case (t *: EmptyTuple) => F[t]
  case _                 => Tuple.Map[Types, F]
}

type HKDNamedElems[Labels <: Tuple, Types <: Tuple, F[_]] = (Labels, Types) match {
  case (l *: EmptyTuple, t *: EmptyTuple) => (l, F[t])
  case _ => Tuple.Zip[Labels, Tuple.Map[Types, F]]
}

opaque type HKDElemsNormalizer[Types <: Tuple, F[_]] = HKDElems[Types, F] => Tuple.Map[Types, F]
opaque type HKDNamedElemsNormalizer[Labels <: Tuple, Types <: Tuple, F[_]] = HKDNamedElems[Labels, Types, F] => Tuple.Map[Types, F]

object HKDElemsNormalizer {
  given [T, F[_]] : HKDElemsNormalizer[T *: EmptyTuple, F] = (input: F[T]) => input *: EmptyTuple
  given [T1, T2, F[_]] : HKDElemsNormalizer[(T1, T2), F] = identity
  given [T1, T2, TS <: NonEmptyTuple, F[_]] (using normalizer: HKDElemsNormalizer[T2 *: TS, F]) : HKDElemsNormalizer[T1 *: T2 *: TS, F] = (elems: F[T1] *: HKDElems[T2 *: TS, F]) => {
    elems.head *: normalizer(elems.tail)
  }

  def normalize[Types <: Tuple, F[_]](in: HKDElems[Types, F])(using normalizer: HKDElemsNormalizer[Types, F]): Tuple.Map[Types, F] = normalizer(in)
}

object HKDNamedElemsNormalizer {
  given [Label, Type, F[_]] : HKDNamedElemsNormalizer[Label *: EmptyTuple, Type *: EmptyTuple, F] = (input: (Label, F[Type])) => {
    input._2 *: EmptyTuple
  }
  given [L1, L2, T1, T2, F[_]] : HKDNamedElemsNormalizer[(L1, L2), (T1, T2), F] = (elems: ((L1, F[T1]), (L2, F[T2]))) => {
    (elems._1._2, elems._2._2)
  }
  given [L1, L2, LS <: NonEmptyTuple, T1, T2, TS <: NonEmptyTuple, F[_]] (using tailNormalizer: HKDNamedElemsNormalizer[L2 *: LS, T2 *: TS, F]) : HKDNamedElemsNormalizer[L1 *: L2 *: LS, T1 *: T2 *: TS, F] = (elems: (L1, F[T1]) *: HKDNamedElems[L2 *: LS, T2 *: TS, F]) => {
    elems.head._2 *: tailNormalizer(elems.tail)
  }

  def normalize[Labels <: Tuple, Types <: Tuple, F[_]](in: HKDNamedElems[Labels, Types, F])(using normalizer: HKDNamedElemsNormalizer[Labels, Types, F]): Tuple.Map[Types, F] = normalizer(in)
}

object HKD extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R], normalizer: HKDElemsNormalizer[mirror.MirroredElemTypes, F])(elems: HKDElems[mirror.MirroredElemTypes, F]): HKD[R, F] = HKD.fromTupleMap(HKDElemsNormalizer.normalize(elems))
  def applyDynamicNamed[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R], normalizer: HKDNamedElemsNormalizer[mirror.MirroredElemLabels, mirror.MirroredElemTypes, F])(elems: HKDNamedElems[mirror.MirroredElemLabels, mirror.MirroredElemTypes, F]): HKD[R, F] = {
    HKD.fromTupleMap(HKDNamedElemsNormalizer.normalize(elems))
  }

  def fromTupleMap[R <: Product, F[_]](using mirror: Mirror.ProductOf[R])(tupleMap: Tuple.Map[mirror.MirroredElemTypes, F]): HKD[R, F] = new HKDImpl(tupleMap)
}
