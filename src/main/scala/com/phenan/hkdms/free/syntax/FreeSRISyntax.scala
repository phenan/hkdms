package com.phenan.hkdms.free.syntax

import com.phenan.hkdms.free.FreeSRI
import com.phenan.hkdms.iso.*
import com.phenan.hkdms.util.IndexedUnion

object FreeSRISyntax {
  def impure [F[_], A] (effect: F[A]): FreeSRI[F, A] = FreeSRI.Impure(effect)

  def nil [F[_]]: TupleMapBuilder[F, EmptyTuple] = new TupleMapBuilder(EmptyTuple)
  def product [F[_], T <: Tuple](builder: TupleMapBuilder[F, T]): FreeSRI[F, T] = builder.product
  def union [F[_], T <: Tuple](builder: TupleMapBuilder[F, T]): FreeSRI[F, IndexedUnion[T]] = builder.union

  def struct[Struct]: StructBuilder[Struct] = new StructBuilder[Struct]
  def union[Sum]: UnionBuilder[Sum] = new UnionBuilder[Sum]

  class TupleMapBuilder[F[_], T <: Tuple](tuple: Tuple.Map[T, [t] =>> FreeSRI[F, t]]) {
    def *: [A] (elem: FreeSRI[F, A]): TupleMapBuilder[F, A *: T] = new TupleMapBuilder[F, A *: T](elem *: tuple)
    def product: FreeSRI[F, T] = FreeSRI.Product(tuple)
    def union: FreeSRI[F, IndexedUnion[T]] = FreeSRI.Union(tuple)
  }

  class StructBuilder[Struct] {
    def apply [F[_], T <: Tuple](builder: TupleMapBuilder[F, T])(using iso: T <=> Struct): FreeSRI[F, Struct] = builder.product.iMap(iso)
  }

  class UnionBuilder[Sum] {
    def apply [F[_], T <: Tuple](builder: TupleMapBuilder[F, T])(using iso: IndexedUnion[T] <=> Sum): FreeSRI[F, Sum] = builder.union.iMap(iso)
  }
}
