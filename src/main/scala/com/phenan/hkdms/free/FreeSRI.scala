package com.phenan.hkdms.free

import com.phenan.hkdms.iso.*
import com.phenan.hkdms.iso.given
import com.phenan.hkdms.SemiringalInvariant
import com.phenan.hkdms.util.TupleMaps

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
  case class Union[F[_], T <: Tuple] (tuple: Tuple.Map[T, [t] =>> FreeSRI[F, t]]) extends FreeSRI[F, Tuple.Union[T]] {
    def foldMap[G[_]](f: [a] => F[a] => G[a])(using semiringalInvariant: SemiringalInvariant[G]): G[Tuple.Union[T]] = {
      semiringalInvariant.sum[T](TupleMaps.map(tuple)([t] => (ft: FreeSRI[F, t]) => ft.foldMap(f)))
    }
  }
  case class IMapped[F[_], A, B] (fa: FreeSRI[F, A], iso: A <=> B) extends FreeSRI[F, B] {
    def foldMap[G[_]](f: [a] => F[a] => G[a])(using semiringalInvariant: SemiringalInvariant[G]): G[B] = {
      semiringalInvariant.imap(iso)(fa.foldMap(f))
    }
  }

  class TupleMapBuilder[F[_], T <: Tuple](tuple: Tuple.Map[T, [t] =>> FreeSRI[F, t]]) {
    def *: [A] (elem: FreeSRI[F, A]): TupleMapBuilder[F, A *: T] = new TupleMapBuilder[F, A *: T](elem *: tuple)
    def product: FreeSRI[F, T] = Product(tuple)
    def union: FreeSRI[F, Tuple.Union[T]] = Union(tuple)
  }

  def nil [F[_]]: TupleMapBuilder[F, EmptyTuple] = new TupleMapBuilder(EmptyTuple)
  def product [F[_], T <: Tuple](builder: TupleMapBuilder[F, T]): FreeSRI[F, T] = builder.product
  def union [F[_], T <: Tuple](builder: TupleMapBuilder[F, T]): FreeSRI[F, Tuple.Union[T]] = builder.union

  def struct[Struct]: StructBuilder[Struct] = new StructBuilder[Struct]

  class StructBuilder[Struct] {
    def apply [F[_], T <: Tuple](builder: TupleMapBuilder[F, T])(using iso: T <=> Struct): FreeSRI[F, Struct] = builder.product.iMap(iso)
  }

  def union[Sum]: UnionBuilder[Sum] = new UnionBuilder[Sum]

  class UnionBuilder[Sum] {
    def apply [F[_], T <: Tuple](builder: TupleMapBuilder[F, T])(using iso: Tuple.Union[T] <=> Sum): FreeSRI[F, Sum] = builder.union.iMap(iso)
  }
}
