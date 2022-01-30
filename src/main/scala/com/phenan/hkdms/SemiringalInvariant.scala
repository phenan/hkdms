package com.phenan.hkdms

import com.phenan.hkdms.iso.*
import com.phenan.hkdms.util.IndexedUnion

trait SemiringalInvariant [F[_]] {
  def pure [A] (a: => A): F[A]
  def product [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[T]
  def sum [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[IndexedUnion[T]]

  def imap [A, B] (iso: A <=> B): F[A] => F[B]
}
