package com.phenan.hkdms.hkd

import cats.{InvariantMonoidal, SemigroupK}
import com.phenan.hkdms.InvariantSemiringal
import com.phenan.hkdms.syntax.*
import com.phenan.hkdms.util.{IndexedUnion, TupleMaps}

import scala.deriving.Mirror
import scala.language.dynamics

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

object HKProduct extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(args: Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKForest[e, F]]): HKProduct[R, F] = new HKProductImpl(HKD(HKDElems.fromTupleMap(args)))
  def applyDynamicNamed[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(params: Tuple.Zip[mirror.MirroredElemLabels, Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKForest[e, F]]]): HKProduct[R, F] = {
    val args = for (i <- 0 until params.size) yield {
      params.productElement(i).asInstanceOf[(_, _)]._2
    }
    new HKProductImpl(HKD(HKDElems.fromTupleMap(Tuple.fromArray(args.toArray).asInstanceOf[Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKForest[e, F]]])))
  }
}

object HKSum extends Dynamic {
  def applyDynamic[R, F[_]](nameApply: "apply")(using mirror: Mirror.SumOf[R])(args: => Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKForest[e, F]]): HKSum[R, F] = new HKSumImpl[R, F](using mirror)(args)
}

given [T, F[_]] : Conversion[HKForest[T, F], Tuple.Map[T *: EmptyTuple, [e] =>> HKForest[e, F]]] = {
  _ *: EmptyTuple
}
