package com.phenan.hkdms.syntax

import cats.InvariantMonoidal

extension [F[_]] (invariantMonoidal: InvariantMonoidal[F]) {
  @annotation.nowarn
  def productAll[T <: Tuple](tupleMap: Tuple.Map[T, F]): F[T] = {
    tupleMap match {
      case (e: F[_]) *: (es: Tuple.Map[_, F]) => {
        val pair = invariantMonoidal.product(e, productAll(es))
        val tupled = invariantMonoidal.imap(pair) {
          case (head, tail) => head *: tail
        } {
          case head *: tail => (head, tail)
        }
        tupled.asInstanceOf[F[T]]
      }
      case _ => invariantMonoidal.point(EmptyTuple.asInstanceOf[T])
    }
  }

  def productR[T](prefix: F[Unit], value: F[T]): F[T] = {
    invariantMonoidal.imap(invariantMonoidal.product(prefix, value))(_._2)(((), _))
  }
  
  def productL[T](value: F[T], postfix: F[Unit]): F[T] = {
    invariantMonoidal.imap(invariantMonoidal.product(value, postfix))(_._1)((_, ()))
  }
}
