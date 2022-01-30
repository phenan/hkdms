package com.phenan.hkdms.free

import com.phenan.hkdms.iso.*
import com.phenan.hkdms.iso.given
import com.phenan.hkdms.SemiringalInvariant
import com.phenan.hkdms.util.{IndexedUnion, TupleMaps}

sealed trait FreeSRI [F[_], T] {
  def foldMap[G[_]](f: [a] => F[a] => G[a])(using SemiringalInvariant[G]): G[T]

  def iMap[U](iso: T <=> U): FreeSRI[F, U] = FreeSRI.IMapped(this, iso)

  def *>: (prefix: FreeSRI[F, Unit]): FreeSRI[F, T] = FreeSRI.IMapped(FreeSRI.Product[F, (Unit, T)]((prefix, this)), summon[(Unit, T) <=> T])
  def :<* (postfix: FreeSRI[F, Unit]): FreeSRI[F, T] = FreeSRI.IMapped(FreeSRI.Product[F, (T, Unit)]((this, postfix)), summon[(T, Unit) <=> T])
}

object FreeSRI {
  case class Pure[F[_], T] (value: T) extends FreeSRI[F, T] {
    def foldMap[G[_]](f: [a] => F[a] => G[a])(using semiringalInvariant: SemiringalInvariant[G]): G[T] = semiringalInvariant.pure(value)
  }
  case class Impure[F[_], T] (effect: F[T]) extends FreeSRI[F, T] {
    def foldMap[G[_]](f: [a] => F[a] => G[a])(using SemiringalInvariant[G]): G[T] = f[T](effect)
  }
  case class Product[F[_], T <: Tuple] (tuple: Tuple.Map[T, [t] =>> FreeSRI[F, t]]) extends FreeSRI[F, T] {
    def foldMap[G[_]](f: [a] => F[a] => G[a])(using semiringalInvariant: SemiringalInvariant[G]): G[T] = {
      semiringalInvariant.product(TupleMaps.map(tuple)([t] => (ft: FreeSRI[F, t]) => ft.foldMap(f)))
    }
  }
  case class Union[F[_], T <: Tuple] (tuple: Tuple.Map[T, [t] =>> FreeSRI[F, t]]) extends FreeSRI[F, IndexedUnion[T]] {
    def foldMap[G[_]](f: [a] => F[a] => G[a])(using semiringalInvariant: SemiringalInvariant[G]): G[IndexedUnion[T]] = {
      semiringalInvariant.sum[T](TupleMaps.map(tuple)([t] => (ft: FreeSRI[F, t]) => ft.foldMap(f)))
    }
  }
  case class IMapped[F[_], A, B] (fa: FreeSRI[F, A], iso: A <=> B) extends FreeSRI[F, B] {
    def foldMap[G[_]](f: [a] => F[a] => G[a])(using semiringalInvariant: SemiringalInvariant[G]): G[B] = {
      semiringalInvariant.imap(iso)(fa.foldMap(f))
    }
  }
}
