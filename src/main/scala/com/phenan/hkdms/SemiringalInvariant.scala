package com.phenan.hkdms

import com.phenan.hkdms.iso.*

trait SemiringalInvariant [F[_]] {
  def pure [A] (a: => A): F[A]
  def product [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[T]
  def sum [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[Tuple.Union[T]]

  def imap [A, B] (iso: A <=> B): F[A] => F[B]
}
