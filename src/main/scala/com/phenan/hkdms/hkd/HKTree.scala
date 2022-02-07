package com.phenan.hkdms.hkd

import cats.{Applicative, Traverse}
import com.phenan.hkdms.syntax.*
import com.phenan.hkdms.util.*

import scala.collection.IterableFactory
import scala.deriving.Mirror
import scala.language.dynamics

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
    applicative.map(applicative.productAll(hkd.map[F]([u] => (hkt: HKTree[u, F]) => hkt.fold).asTuple))(mirror.fromProduct)
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

opaque type HKStructFields[Fields <: Tuple, F[_]] = Tuple.Map[Fields, [e] =>> HKTree[e, F]]

object HKStructFields {
  given [T, F[_]] : Conversion[HKTree[T, F], HKStructFields[T *: EmptyTuple, F]] = _ *: EmptyTuple
  given [T, F[_]] : Conversion[F[T], HKStructFields[T *: EmptyTuple, F]] = HKValue(_) *: EmptyTuple

  def toHKD[R <: Product, F[_]](using mirror: Mirror.ProductOf[R])(fields: HKStructFields[mirror.MirroredElemTypes, F]): HKD[R, [e] =>> HKTree[e, F]] = HKD[R, [e] =>> HKTree[e, F]](fields)
}

object HKStruct extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(args: HKStructFields[mirror.MirroredElemTypes, F]): HKStruct[R, F] = new HKStructImpl(HKStructFields.toHKD(args))
  def applyDynamicNamed[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(params: Tuple.Zip[mirror.MirroredElemLabels, Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKTree[e, F]]]): HKStruct[R, F] = {
    val args = for (i <- 0 until params.size) yield {
      params.productElement(i).asInstanceOf[(_, _)]._2
    }
    new HKStructImpl(HKD(Tuple.fromArray(args.toArray).asInstanceOf[Tuple.Map[mirror.MirroredElemTypes, [e] =>> HKTree[e, F]]]))
  }
}

object HKList {
  def apply[C[_] : Traverse, T, F[_]](trees: C[HKTree[T, F]]): HKList[C, T, F] = new HKListImpl(trees)
  def apply[C[_] : Traverse, T, F[_]](factory: IterableFactory[C])(trees: HKTree[T, F]*): HKList[C, T, F] = new HKListImpl(factory(trees*))
}

given [T, F[_]] : Conversion[F[T], HKValue[T, F]] = HKValue(_)
