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

opaque type HKDElems[T <: Tuple, F[_]] = Tuple.Map[T, F]

object HKDElems {
  given [T, F[_]] : Conversion[F[T], HKDElems[T *: EmptyTuple, F]] = _ *: EmptyTuple
  given [T, F[_]] : Conversion[F[T] *: EmptyTuple, HKDElems[T *: EmptyTuple, F]] = identity
  given [H, S <: Tuple, T <: Tuple, F[_]] (using conv: Conversion[S, HKDElems[T, F]]) : Conversion[F[H] *: S, HKDElems[H *: T, F]] = v => {
    v.head *: conv(v.tail)
  }

  def fromTupleMap[T <: Tuple, F[_]] (tupleMap: Tuple.Map[T, F]): HKDElems[T, F] = tupleMap
  def toTupleMap[T <: Tuple, F[_]] (elems: HKDElems[T, F]): Tuple.Map[T, F] = elems
}

type HKDNamedInput[Labels <: Tuple, Types <: Tuple, F[_]] = (Labels, Types) match {
  case (l *: EmptyTuple, t *: EmptyTuple) => (l, F[t])
  case _ => Tuple.Zip[Labels, Tuple.Map[Types, F]]
}

opaque type HKDNamedInputNormalizer[Labels <: Tuple, Types <: Tuple, F[_]] = HKDNamedInput[Labels, Types, F] => Tuple.Zip[Labels, Tuple.Map[Types, F]]

object HKDNamedInputNormalizer {
  given singleArg [Label, Type, F[_]] : HKDNamedInputNormalizer[Label *: EmptyTuple, Type *: EmptyTuple, F] = (input: HKDNamedInput[Label *: EmptyTuple, Type *: EmptyTuple, F]) => {
    val pair: (Label, F[Type]) = input
    pair *: EmptyTuple
  }
  given moreThanTwoArgs [Label, Type, Labels <: NonEmptyTuple, Types <: NonEmptyTuple, F[_]] : HKDNamedInputNormalizer[Label *: Labels, Type *: Types, F] = identity

  def normalize[Labels <: Tuple, Types <: Tuple, F[_]](in: HKDNamedInput[Labels, Types, F])(using normalizer: HKDNamedInputNormalizer[Labels, Types, F]): Tuple.Zip[Labels, Tuple.Map[Types, F]] = normalizer(in)
}

object HKD extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(args: HKDElems[mirror.MirroredElemTypes, F]): HKD[R, F] = new HKDImpl(HKDElems.toTupleMap(args))
  def applyDynamicNamed[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R], normalizer: HKDNamedInputNormalizer[mirror.MirroredElemLabels, mirror.MirroredElemTypes, F])(in: HKDNamedInput[mirror.MirroredElemLabels, mirror.MirroredElemTypes, F]): HKD[R, F] = {
    val params = HKDNamedInputNormalizer.normalize(in)
    val args = for (i <- 0 until params.size) yield {
      params.productElement(i).asInstanceOf[(_, _)]._2
    }
    new HKDImpl(Tuple.fromArray(args.toArray).asInstanceOf[Tuple.Map[mirror.MirroredElemTypes, F]])
  }
}
