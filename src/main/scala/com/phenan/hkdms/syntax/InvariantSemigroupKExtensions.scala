package com.phenan.hkdms.syntax

import cats.{Invariant, SemigroupK}
import com.phenan.hkdms.util.IndexedUnion

extension [F[_]] (semigroupK: SemigroupK[F])(using invariant: Invariant[F]) {
  def combineAll[T <: Tuple](tupleMap: Tuple.Map[T, F]): F[IndexedUnion[T]] = {
    val indexedUnions: Seq[F[IndexedUnion[T]]] = for (i <- 0 until tupleMap.productArity) yield {
      val value = tupleMap.productElement(i).asInstanceOf[F[Tuple.Union[T]]]
      invariant.imap(value)(union => IndexedUnion[T](union, i))(_.value)
    }
    indexedUnions.reduce(semigroupK.combineK)
  }
}
