package com.phenan.hkdms.util

sealed trait IndexedUnion [T <: Tuple] {
  def value: Tuple.Union[T]
  def fold[R](functions: Tuple.Map[T, [t] =>> t => R]): R
}

object IndexedUnion {
  def apply[T <: Tuple](value: Tuple.Union[T], index: Int): IndexedUnion[T] = {
    new NonEmpty(value, index)
  }

  object Nil extends IndexedUnion[EmptyTuple] {
    def value: Tuple.Union[EmptyTuple] = {
      throw new RuntimeException("empty union")
    }
    def fold[R](functions: Tuple.Map[EmptyTuple, [t] =>> t => R]): R = {
      throw new RuntimeException("empty union")
    }
  }

  class NonEmpty [T <: Tuple] (val value: Tuple.Union[T], index: Int) extends IndexedUnion[T] {
    def fold[R](functions: Tuple.Map[T, [t] =>> t => R]): R = {
      val func = functions.productElement(index).asInstanceOf[Tuple.Union[T] => R]
      func(value)
    }
  }
}
