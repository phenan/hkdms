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

opaque type HKStructField[T, F[_]] = HKTree[T, F]

object HKStructField {
  given [T, F[_]] : Conversion[HKTree[T, F], HKStructField[T, F]] = identity
  given [T, F[_]] : Conversion[F[T], HKStructField[T, F]] = HKValue(_)
}

opaque type HKStructFields[T <: Tuple, F[_]] = Tuple.Map[T, [e] =>> HKStructField[e, F]]

object HKStructFields {
  given [A, H, F[_]] (using headConv: Conversion[A, HKStructField[H, F]]): Conversion[A, HKStructFields[H *: EmptyTuple, F]] = {
    (a: A) => headConv(a) *: EmptyTuple
  }
  given [A, H, F[_]] (using headConv: Conversion[A, HKStructField[H, F]]): Conversion[A *: EmptyTuple, HKStructFields[H *: EmptyTuple, F]] = {
    (a: A *: EmptyTuple) => headConv(a.head) *: EmptyTuple
  }
  given [A, B <: Tuple, H, T <: Tuple, F[_]] (using headConv: Conversion[A, HKStructField[H, F]], tailConv: Conversion[B, HKStructFields[T, F]]): Conversion[A *: B, HKStructFields[H *: T, F]] = {
    (ab: A *: B) => headConv(ab.head) *: tailConv(ab.tail)
  }
}

extension [T <: Tuple, F[_]] (fields: HKStructFields[T, F]) {
  def toTupleMap: Tuple.Map[T, [e] =>> HKTree[e, F]] = fields
}

object HKStruct extends Dynamic {
  def applyDynamic[R <: Product, F[_]](nameApply: "apply")(using mirror: Mirror.ProductOf[R])(args: HKStructFields[mirror.MirroredElemTypes, F]): HKStruct[R, F] = new HKStructImpl(HKD(args.toTupleMap))
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

given [T, F[_]] : Conversion[HKTree[T, F], Tuple.Map[T *: EmptyTuple, [e] =>> HKTree[e, F]]] = _ *: EmptyTuple
given [T, F[_]] : Conversion[F[T], HKValue[T, F]] = HKValue(_)
