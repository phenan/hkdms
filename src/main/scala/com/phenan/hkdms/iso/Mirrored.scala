package com.phenan.hkdms.iso

import com.phenan.hkdms.util.IndexedUnion

import scala.deriving.*

given [T <: Product, U <: Tuple] (using mirror: Mirror.ProductOf[T], proof: mirror.MirroredElemTypes =:= U): Iso[T, U] = {
  val to = (value: T) => proof(Tuple.fromProductTyped(value))
  val from = (underlying: U) => mirror.fromProduct(underlying)
  Iso(to, from)
}

given [T, U <: Tuple] (using mirror: Mirror.SumOf[T], proof: mirror.MirroredElemTypes =:= U): Iso[T, IndexedUnion[U]] = {
  val to = (value: T) => IndexedUnion(value.asInstanceOf[Tuple.Union[U]], mirror.ordinal(value))
  val from = (underlying: IndexedUnion[U]) => underlying.value.asInstanceOf[T]
  Iso(to, from)
}
