package com.phenan.hkdms.iso

import scala.deriving.*

given [T <: Product, U <: Tuple] (using mirror: Mirror.ProductOf[T], proof: mirror.MirroredElemTypes =:= U): Iso[T, U] = {
  val to = (value: T) => mirror.fromProduct(value).asInstanceOf[U]
  val from = (underlying: U) => Tuple.fromProduct(underlying).asInstanceOf[T]
  Iso(to, from)
}

given [T, U <: Tuple] (using mirror: Mirror.SumOf[T], proof: mirror.MirroredElemTypes =:= U): Iso[T, Tuple.Union[U]] = {
  val to = (value: T) => value.asInstanceOf[Tuple.Union[U]]
  val from = (underlying: Tuple.Union[U]) => underlying.asInstanceOf[T]
  Iso(to, from)
}
