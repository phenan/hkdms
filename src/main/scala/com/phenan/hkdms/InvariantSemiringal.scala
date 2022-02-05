package com.phenan.hkdms

import cats.Alternative
import com.phenan.hkdms.iso.*
import com.phenan.hkdms.syntax.*
import com.phenan.hkdms.util.IndexedUnion

trait InvariantSemiringal [F[_]] {
  def pure [A] (a: => A): F[A]
  def product [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[T]
  def sum [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[IndexedUnion[T]]

  def imap [A, B] (iso: A <=> B): F[A] => F[B]
}

object InvariantSemiringal {
  given [F[_]] (using alternative: Alternative[F]): InvariantSemiringal[F] = new InvariantSemiringal[F] {
    override def pure[A](a: => A): F[A] = alternative.pure(a)
    override def product[T <: Tuple](tupleMap: Tuple.Map[T, F]): F[T] = alternative.productAll(tupleMap)
    override def sum[T <: Tuple](tupleMap: Tuple.Map[T, F]): F[IndexedUnion[T]] = alternative.combineAll(tupleMap)
    override def imap[A, B](iso: A <=> B): F[A] => F[B] = fa => alternative.imap(fa)(iso.to)(iso.from)
  }
}
