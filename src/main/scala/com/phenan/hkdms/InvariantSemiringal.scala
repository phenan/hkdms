package com.phenan.hkdms

import cats.{Alternative, InvariantMonoidal}
import com.phenan.hkdms.util.*

trait InvariantSemiringal [F[_]] extends InvariantMonoidal[F] {
  def pure [A] (a: => A): F[A]
  def productAll [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[T]
  def sumAll [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[IndexedUnion[T]]

  def imap[A, B] (fa: F[A])(f: A => B)(g: B => A): F[B]

  override def unit: F[Unit] = pure(())
  override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = productAll((fa, fb))
}

object InvariantSemiringal {
  given [F[_]] (using alternative: Alternative[F]): InvariantSemiringal[F] with {
    override def pure[A](a: => A): F[A] = alternative.pure(a)
    override def productAll[T <: Tuple](tupleMap: Tuple.Map[T, F]): F[T] = alternative.productAll(tupleMap)
    override def sumAll[T <: Tuple](tupleMap: Tuple.Map[T, F]): F[IndexedUnion[T]] = alternative.combineAll(tupleMap)
    override def imap[A, B] (fa: F[A])(f: A => B)(g: B => A): F[B] = alternative.imap(fa)(f)(g)
  }
}
