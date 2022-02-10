package com.phenan.hkdms.hkd

import cats.{Applicative, Traverse}
import com.phenan.hkdms.syntax.*
import com.phenan.hkdms.util.*

import scala.collection.IterableFactory
import scala.deriving.Mirror
import scala.language.dynamics
import scala.reflect.TypeTest

sealed trait HKTree [R, F[_]] {
  def map [U] (f: R => U) : HKTree[U, F] = new HKMapped(this, f)

  def hmap [G[_]](f: [t] => F[t] => G[t]): HKTree[R, G]

  def widen [U >: R] : HKTree[U, F] = map(identity)

  def fold (using applicative: Applicative[F]): F[R]

  def foldMap [G[_]](compiler: [t] => F[t] => G[t])(using applicative: Applicative[G]): G[R] = hmap(compiler).fold
}

trait HKStruct [R <: Product, F[_]] extends HKTree[R, F] {
  def hmap[G[_]](f: [t] => F[t] => G[t]): HKStruct[R, G]
}

trait HKList [C[_], R, F[_]] (using traverse: Traverse[C]) extends HKTree[C[R], F] {
  def hmap[G[_]](f: [t] => F[t] => G[t]): HKList[C, R, G]
}

class HKMapped [T, R, F[_]] (tree: HKTree[T, F], mapper: T => R) extends HKTree[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKMapped[T, R, G] = {
    new HKMapped(tree.hmap(f), mapper)
  }
  def fold (using applicative: Applicative[F]): F[R] = {
    applicative.map(tree.fold)(mapper)
  }
}

case class HKValue [R, F[_]] (value: F[R]) extends HKTree[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKValue[R, G] = HKValue(f[R](value))
  def fold (using applicative: Applicative[F]): F[R] = value
}

private class HKStructImpl [R <: Product, F[_]] (hkd: HKD[R, [e] =>> HKTree[e, F]])(using mirror: Mirror.ProductOf[R]) extends HKStruct[R, F] {
  def hmap [G[_]](f: [t] => F[t] => G[t]): HKStruct[R, G] = {
    new HKStructImpl(hkd.map([u] => (hkt: HKTree[u, F]) => hkt.hmap(f)))
  }
  def fold (using applicative: Applicative[F]): F[R] = {
    hkd.foldMap([u] => (hkt: HKTree[u, F]) => hkt.fold)
  }
}

private class HKListImpl [C[_], R, F[_]] (trees: C[HKTree[R, F]])(using traverse: Traverse[C]) extends HKList[C, R, F] {
  def hmap[G[_]](f: [t] => F[t] => G[t]): HKList[C, R, G] = {
    new HKListImpl(traverse.map(trees)(_.hmap(f)))
  }
  def fold (using applicative: Applicative[F]): F[C[R]] = {
    traverse.traverse(trees) { _.fold }
  }
}

type HKStructField[E, F[_]] = HKTree[E, F] | F[E]

type HKStructFields[T <: Tuple, F[_]] = T match {
  case t *: EmptyTuple => HKStructField[t, F]
  case _ => Tuple.Map[T, [e] =>> HKStructField[e, F]]
}

type HKStructNamedFields[Labels <: Tuple, Types <: Tuple, F[_]] = (Labels, Types) match {
  case (l *: EmptyTuple, t *: EmptyTuple) => (l, HKStructField[t, F])
  case _ => Tuple.Zip[Labels, Tuple.Map[Types, [e] =>> HKStructField[e, F]]]
}

opaque type HKStructFieldNormalizer[E, F[_]] = HKStructField[E, F] => HKTree[E, F]
opaque type HKStructFieldsNormalizer[T <: Tuple, F[_]] = HKStructFields[T, F] => Tuple.Map[T, [e] =>> HKTree[e, F]]
opaque type HKStructNamedFieldsNormalizer[Labels <: Tuple, Types <: Tuple, F[_]] = HKStructNamedFields[Labels, Types, F] => Tuple.Map[Types, [e] =>> HKTree[e, F]]

object HKStructFieldNormalizer {
  given [E, F[_]] (using typeTest: TypeTest[Any, HKTree[_, _]]) : HKStructFieldNormalizer[E, F] = (field: HKStructField[E, F]) => {
    field match {
      case typeTest(tree) => tree.asInstanceOf[HKTree[E, F]]
      case _ => HKValue(field.asInstanceOf[F[E]])
    }
  }
  def normalize[E, F[_]](field: HKStructField[E, F])(using normalizer: HKStructFieldNormalizer[E, F]): HKTree[E, F] = normalizer(field)
}

object HKStructFieldsNormalizer {
  given [E, F[_]] (using HKStructFieldNormalizer[E, F]) : HKStructFieldsNormalizer[E *: EmptyTuple, F] = (field: HKStructField[E, F]) => {
    HKStructFieldNormalizer.normalize(field) *: EmptyTuple
  }
  given [E1, E2, F[_]] (using HKStructFieldNormalizer[E1, F], HKStructFieldNormalizer[E2, F]) : HKStructFieldsNormalizer[(E1, E2), F] = (fields: (HKStructField[E1, F], HKStructField[E2, F])) => {
    (HKStructFieldNormalizer.normalize(fields._1), HKStructFieldNormalizer.normalize(fields._2))
  }
  given [E1, E2, ES <: NonEmptyTuple, F[_]] (using headNormalizer: HKStructFieldNormalizer[E1, F], tailNormailzer: HKStructFieldsNormalizer[E2 *: ES, F]) : HKStructFieldsNormalizer[E1 *: E2 *: ES, F] = (fields: HKStructField[E1, F] *: HKStructFields[E2 *: ES, F]) => {
    HKStructFieldNormalizer.normalize(fields.head) *: tailNormailzer(fields.tail)
  }
  def normalize[T <: Tuple, F[_]](fields: HKStructFields[T, F])(using normalizer: HKStructFieldsNormalizer[T, F]): Tuple.Map[T, [e] =>> HKTree[e, F]] = normalizer(fields)
}

object HKStructNamedFieldsNormalizer {
  given [L, E, F[_]] (using HKStructFieldNormalizer[E, F]) : HKStructNamedFieldsNormalizer[L *: EmptyTuple, E *: EmptyTuple, F] = (pair: (L, HKStructField[E, F])) => {
    HKStructFieldNormalizer.normalize(pair._2) *: EmptyTuple
  }
  given [L1, L2, E1, E2, F[_]] (using HKStructFieldNormalizer[E1, F], HKStructFieldNormalizer[E2, F]) : HKStructNamedFieldsNormalizer[(L1, L2), (E1, E2), F] = (fields: ((L1, HKStructField[E1, F]), (L2, HKStructField[E2, F]))) => {
    (HKStructFieldNormalizer.normalize(fields._1._2), HKStructFieldNormalizer.normalize(fields._2._2))
  }
  given [L1, L2, LS <: NonEmptyTuple, E1, E2, ES <: NonEmptyTuple, F[_]] (using headNormalizer: HKStructFieldNormalizer[E1, F], tailNormailzer: HKStructNamedFieldsNormalizer[L2 *: LS, E2 *: ES, F]) : HKStructNamedFieldsNormalizer[L1 *: L2 *: LS, E1 *: E2 *: ES, F] = {
    (fields: (L1, HKStructField[E1, F]) *: HKStructNamedFields[L2 *: LS, E2 *: ES, F]) => HKStructFieldNormalizer.normalize(fields.head._2) *: tailNormailzer(fields.tail)
  }
  def normalize[L <: Tuple, T <: Tuple, F[_]](fields: HKStructNamedFields[L, T, F])(using normalizer: HKStructNamedFieldsNormalizer[L, T, F]): Tuple.Map[T, [e] =>> HKTree[e, F]] = normalizer(fields)
}

object HKTree {
  given [Sub, Super >: Sub, F[_]]: Conversion[HKTree[Sub, F], HKTree[Super, F]] = _.widen
}

object HKStruct extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R], normalizer: HKStructFieldsNormalizer[mirror.MirroredElemTypes, F])(fields: HKStructFields[mirror.MirroredElemTypes, F]): HKStruct[R, F] = {
    new HKStructImpl(HKD.fromTupleMap(HKStructFieldsNormalizer.normalize(fields)))
  }
  def applyDynamicNamed[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R], normalizer: HKStructNamedFieldsNormalizer[mirror.MirroredElemLabels, mirror.MirroredElemTypes, F])(fields: HKStructNamedFields[mirror.MirroredElemLabels, mirror.MirroredElemTypes, F]): HKStruct[R, F] = {
    new HKStructImpl(HKD.fromTupleMap(HKStructNamedFieldsNormalizer.normalize(fields)))
  }
}

object HKList {
  def apply[C[_] : Traverse, T, F[_]](trees: C[HKTree[T, F]]): HKList[C, T, F] = new HKListImpl(trees)
  def apply[C[_] : Traverse, T, F[_]](factory: IterableFactory[C])(trees: HKTree[T, F]*): HKList[C, T, F] = new HKListImpl(factory(trees*))
}
