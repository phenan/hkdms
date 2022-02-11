package com.phenan.hkdms

import cats.{InvariantMonoidal, SemigroupK}
import com.phenan.hkdms.util.*

import scala.deriving.Mirror
import scala.language.dynamics
import scala.reflect.TypeTest

sealed trait HKForest [R, F[_]] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKForest[R, G]
  def fold (using invariantSemiringal: InvariantSemiringal[F]): F[R]

  def foldMap [G[_]](compiler: [t] => F[t] => G[t])(using invariantSemiringal: InvariantSemiringal[G]): G[R] = hmap(compiler).fold
}

extension [R, F[_]] (forest: => HKForest[R, F]) {
  def *>: (prefix: HKForest[Unit, F]): HKForest[R, F] = new HKPrefixed(prefix, forest)
  def :<* (postfix: HKForest[Unit, F]): HKForest[R, F] = new HKPostfixed(forest, postfix)
}

trait HKProduct [R <: Product, F[_]] extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G]
}

trait HKSum [R, F[_]] extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKSum[R, G]
}

class HKPrefixed [R, F[_]] (prefix: => HKForest[Unit, F], forest: HKForest[R, F]) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKPrefixed[R, G] = new HKPrefixed[R, G](prefix.hmap(f), forest.hmap(f))
  def fold (using invariantSemiringal: InvariantSemiringal[F]): F[R] = {
    invariantSemiringal.productR(prefix.fold, forest.fold)
  }
}

class HKPostfixed [R, F[_]] (forest: => HKForest[R, F], postfix: HKForest[Unit, F]) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKPostfixed[R, G] = new HKPostfixed[R, G](forest.hmap(f), postfix.hmap(f))
  def fold (using invariantSemiringal: InvariantSemiringal[F]): F[R] = {
    invariantSemiringal.productL(forest.fold, postfix.fold)
  }
}

case class HKLeaf [R, F[_]] (value: F[R]) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKLeaf[R, G] = HKLeaf(f[R](value))
  def fold (using invariantSemiringal: InvariantSemiringal[F]): F[R] = value
}

private class HKProductImpl [R <: Product, F[_]] (hkd: HKD[R, [e] =>> HKForest[e, F]])(using mirror: Mirror.ProductOf[R]) extends HKProduct[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G] = {
    new HKProductImpl[R, G](hkd.map([u] => (hkt: HKForest[u, F]) => hkt.hmap(f)))
  }
  def fold (using invariantSemiringal: InvariantSemiringal[F]): F[R] = {
    hkd.foldMap([u] => (hkt: HKForest[u, F]) => hkt.fold)
  }
}

private class HKSumImpl [R, F[_]] (using mirror: Mirror.SumOf[R])(sum: => Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKForest[e, F]]) extends HKSum[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKSum[R, G] = {
    new HKSumImpl[R, G](using mirror)(TupleMaps.map(sum) { [a] => (forest: HKForest[a, F]) => forest.hmap(f) })
  }
  def fold (using invariantSemiringal: InvariantSemiringal[F]): F[R] = {
    val elems: Tuple.Map[mirror.MirroredElemTypes, F] = TupleMaps.map(sum) { [a] => (forest: HKForest[a, F]) => forest.fold }
    invariantSemiringal.imap[IndexedUnion[mirror.MirroredElemTypes], R](invariantSemiringal.sumAll(elems))(_.value.asInstanceOf[R])(r => IndexedUnion(r.asInstanceOf[Tuple.Union[mirror.MirroredElemTypes]], mirror.ordinal(r)))
  }
}

type HKProductElem[T, F[_]] = HKForest[T, F] | F[T]

type HKProductElems[T <: Tuple, F[_]] = T match {
  case t *: EmptyTuple => HKProductElem[t, F]
  case _ => Tuple.Map[T, [e] =>> HKProductElem[e, F]]
}

type HKProductNamedElems[L <: Tuple, T <: Tuple, F[_]] = (L, T) match {
  case (l *: EmptyTuple, t *: EmptyTuple) => (l, HKProductElem[t, F])
  case _ => Tuple.Zip[L, Tuple.Map[T, [e] =>> HKProductElem[e, F]]]
}

opaque type HKProductElemNormalizer[T, F[_]] = HKProductElem[T, F] => HKForest[T, F]
opaque type HKProductElemsNormalizer[T <: Tuple, F[_]] = HKProductElems[T, F] => Tuple.Map[T, [e] =>> HKForest[e, F]]
opaque type HKProductNamedElemsNormalizer[L <: Tuple, T <: Tuple, F[_]] = HKProductNamedElems[L, T, F] => Tuple.Map[T, [e] =>> HKForest[e, F]]

object HKProductElemNormalizer {
  given [E, F[_]] (using typeTest: TypeTest[Any, HKForest[_, _]]): HKProductElemNormalizer[E, F] = {
    case typeTest(forest) => forest.asInstanceOf[HKForest[E, F]]
    case otherwise        => HKLeaf(otherwise.asInstanceOf[F[E]])
  }
  def normalize[E, F[_]](elem: HKProductElem[E, F])(using normalizer: HKProductElemNormalizer[E, F]): HKForest[E, F] = normalizer(elem)
}

object HKProductElemsNormalizer {
  given [E, F[_]] (using HKProductElemNormalizer[E, F]): HKProductElemsNormalizer[E *: EmptyTuple, F] = (elem: HKProductElem[E, F]) => {
    HKProductElemNormalizer.normalize(elem) *: EmptyTuple
  }
  given [E1, E2, F[_]] (using HKProductElemNormalizer[E1, F], HKProductElemNormalizer[E2, F]): HKProductElemsNormalizer[(E1, E2), F] = (elems: (HKProductElem[E1, F], HKProductElem[E2, F])) => {
    (HKProductElemNormalizer.normalize(elems._1), HKProductElemNormalizer.normalize(elems._2))
  }
  given [E1, E2, ES <: NonEmptyTuple, F[_]] (using HKProductElemNormalizer[E1, F], HKProductElemsNormalizer[E2 *: ES, F]): HKProductElemsNormalizer[E1 *: E2 *: ES, F] = (elems: HKProductElem[E1, F] *: HKProductElems[E2 *: ES, F]) => {
    HKProductElemNormalizer.normalize(elems.head) *: HKProductElemsNormalizer.normalize[E2 *: ES, F](elems.tail)
  }
  def normalize[T <: Tuple, F[_]](elems: HKProductElems[T, F])(using normalizer: HKProductElemsNormalizer[T, F]): Tuple.Map[T, [e] =>> HKForest[e, F]] = normalizer(elems)
}

object HKProductNamedElemsNormalizer {
  given [L, E, F[_]] (using HKProductElemNormalizer[E, F]): HKProductNamedElemsNormalizer[L *: EmptyTuple, E *: EmptyTuple, F] = (elem: (L, HKProductElem[E, F])) => {
    HKProductElemNormalizer.normalize(elem._2) *: EmptyTuple
  }
  given [L1, L2, E1, E2, F[_]] (using HKProductElemNormalizer[E1, F], HKProductElemNormalizer[E2, F]): HKProductNamedElemsNormalizer[(L1, L2), (E1, E2), F] = (elems: ((L1, HKProductElem[E1, F]), (L2, HKProductElem[E2, F]))) => {
    (HKProductElemNormalizer.normalize(elems._1._2), HKProductElemNormalizer.normalize(elems._2._2))
  }
  given [L1, L2, LS <: NonEmptyTuple, E1, E2, ES <: NonEmptyTuple, F[_]] (using HKProductElemNormalizer[E1, F], HKProductNamedElemsNormalizer[L2 *: LS, E2 *: ES, F]): HKProductNamedElemsNormalizer[L1 *: L2 *: LS, E1 *: E2 *: ES, F] = {
    (elems: (L1, HKProductElem[E1, F]) *: HKProductNamedElems[L2 *: LS, E2 *: ES, F]) => HKProductElemNormalizer.normalize(elems.head._2) *: HKProductNamedElemsNormalizer.normalize[L2 *: LS, E2 *: ES, F](elems.tail)
  }
  def normalize[L <: Tuple, T <: Tuple, F[_]](elems: HKProductNamedElems[L, T, F])(using normalizer: HKProductNamedElemsNormalizer[L, T, F]): Tuple.Map[T, [e] =>> HKForest[e, F]] = normalizer(elems)
}

object HKProduct extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R], normalizer: HKProductElemsNormalizer[mirror.MirroredElemTypes, F])(args: HKProductElems[mirror.MirroredElemTypes, F]): HKProduct[R, F] = {
    new HKProductImpl(HKD.fromTupleMap(HKProductElemsNormalizer.normalize(args)))
  }
  def applyDynamicNamed[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R], normalizer: HKProductNamedElemsNormalizer[mirror.MirroredElemLabels, mirror.MirroredElemTypes, F])(args: HKProductNamedElems[mirror.MirroredElemLabels, mirror.MirroredElemTypes, F]): HKProduct[R, F] = {
    new HKProductImpl(HKD.fromTupleMap(HKProductNamedElemsNormalizer.normalize(args)))
  }
}

object HKSum extends Dynamic {
  def applyDynamic[R, F[_]](nameApply: "apply")(using mirror: Mirror.SumOf[R])(args: => Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKForest[e, F]]): HKSum[R, F] = new HKSumImpl[R, F](using mirror)(args)
}
