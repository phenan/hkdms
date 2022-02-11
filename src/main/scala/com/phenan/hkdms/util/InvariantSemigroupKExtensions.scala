package com.phenan.hkdms.util

import cats.{Functor, SemigroupK}

extension [F[_]] (semigroupK: SemigroupK[F])(using functor: Functor[F]) {
  def combineAll[T <: Tuple](tupleMap: Tuple.Map[T, F]): F[IndexedUnion[T]] = {
    val indexedUnions: Seq[F[IndexedUnion[T]]] = for (i <- 0 until tupleMap.productArity) yield {
      val value = tupleMap.productElement(i).asInstanceOf[F[Tuple.Elem[T, i.type]]]
      functor.map(value) { (elem: Tuple.Elem[T, i.type]) => IndexedUnion[T](elem.asInstanceOf[Tuple.Union[T]], i) }
    }
    indexedUnions.reduce(semigroupK.combineK)
  }
}
