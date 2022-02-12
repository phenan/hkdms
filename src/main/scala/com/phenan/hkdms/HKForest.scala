package com.phenan.hkdms

import cats.{Defer, InvariantMonoidal, SemigroupK}
import cats.data.NonEmptyList
import com.phenan.hkdms.util.*

import scala.deriving.Mirror
import scala.language.dynamics
import scala.reflect.TypeTest

sealed trait HKForest [R, F[_]] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKForest[R, G]
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R]

  def foldMap [G[_] : InvariantSemiringal : Defer](compiler: [t] => F[t] => G[t]): G[R] = hmap(compiler).fold

  def imap [U] (to: R => U)(from: U => R): HKForest[U, F] = new HKIMapped(this, to, from)

  def optional: HKForest[Option[R], F] = HKSum[Option[R], F](
    HKPure[None.type, F](None),
    imap[Some[R]](Some(_))(_.value)
  )

  def rep0: HKForest[List[R], F] = HKFix { f =>
    HKSum[List[R], F](
      HKProduct[::[R], F](this, f),
      HKPure(Nil)
    )
  }

  def rep1: HKForest[NonEmptyList[R], F] = HKProduct[NonEmptyList[R], F](
    head = this,
    tail = rep0
  )

  def *>: (prefix: HKProductElem[Unit, F])(using HKProductElemNormalizer[Unit, F]): HKForest[R, F] = new HKPrefixed(HKProductElemNormalizer.normalize(prefix), this)
  def :<* (postfix: HKProductElem[Unit, F])(using HKProductElemNormalizer[Unit, F]): HKForest[R, F] = new HKPostfixed(this, HKProductElemNormalizer.normalize(postfix))
}

trait HKProduct [R <: Product, F[_]] extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G]
}

trait HKSum [R, F[_]] extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKSum[R, G]
}

class HKPrefixed [R, F[_]] (prefix: => HKForest[Unit, F], forest: HKForest[R, F]) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKPrefixed[R, G] = new HKPrefixed[R, G](prefix.hmap(f), forest.hmap(f))
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R] = {
    invariantSemiringal.productR(prefix.fold, forest.fold)
  }
}

class HKPostfixed [R, F[_]] (forest: => HKForest[R, F], postfix: HKForest[Unit, F]) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKPostfixed[R, G] = new HKPostfixed[R, G](forest.hmap(f), postfix.hmap(f))
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R] = {
    invariantSemiringal.productL(forest.fold, postfix.fold)
  }
}

case class HKPure [R, F[_]] (value: R) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKPure[R, G] = HKPure(value)
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R] = invariantSemiringal.pure(value)
}

case class HKValue [R, F[_]] (value: F[R]) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKValue[R, G] = HKValue(f[R](value))
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R] = value
}

class HKRef [R] (private[hkdms] val value: Any)

class HKDeref [R, F[_]] (ref: HKRef[R]) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]) = HKDeref(ref)
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R] = {
    // NOTE: hetero map 実装して environment として引き回せば型安全になりそうだけど、そこまでするよりキャストのほうが楽
    ref.value.asInstanceOf[F[R]]
  }
}

class HKFix [R, F[_]] private (fn: (=> HKRef[R]) => HKForest[R, F]) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]) = new HKFix[R, G]((ref) => fn(ref).hmap(f))
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R] = {
    lazy val fixed: F[R] = fn(HKRef(defer.defer(fixed))).fold
    fixed
  }
}

class HKIMapped [T, R, F[_]] (forest: HKForest[T, F], to: T => R, from: R => T) extends HKForest[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKIMapped[T, R, G] = new HKIMapped[T, R, G](forest.hmap(f), to, from)
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R] = invariantSemiringal.imap(forest.fold)(to)(from)
}

private class HKProductImpl [R <: Product, F[_]] (hkd: HKD[R, [e] =>> HKForest[e, F]])(using mirror: Mirror.ProductOf[R]) extends HKProduct[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKProduct[R, G] = {
    new HKProductImpl[R, G](hkd.map([u] => (hkt: HKForest[u, F]) => hkt.hmap(f)))
  }
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R] = {
    hkd.foldMap([u] => (hkt: HKForest[u, F]) => hkt.fold)
  }
}

private class HKSumImpl [R, F[_]] (using mirror: Mirror.SumOf[R])(sum: => Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKForest[e, F]]) extends HKSum[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKSum[R, G] = {
    new HKSumImpl[R, G](using mirror)(TupleMaps.map(sum) { [a] => (forest: HKForest[a, F]) => forest.hmap(f) })
  }
  def fold (using invariantSemiringal: InvariantSemiringal[F], defer: Defer[F]): F[R] = {
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
    case otherwise        => HKValue(otherwise.asInstanceOf[F[E]])
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

object HKFix {
  def apply[R, F[_]](f: (=> HKForest[R, F]) => HKForest[R, F]): HKForest[R, F] = new HKFix(ref => f(HKDeref[R, F](ref)))
}
