package com.phenan.hkdms.util

case class IndexedUnion [T <: Tuple] (val value: Tuple.Union[T], index: Int) {
  def fold[R](functions: Tuple.Map[T, [t] =>> t => R]): R = {
    val func = functions.productElement(index).asInstanceOf[Tuple.Union[T] => R]
    func(value)
  }
}
