package com.phenan.hkdms.util

import cats.Monoid

import scala.compiletime.ops.int.S

object Tuples {
  type IndexOf[T <: Tuple, E] <: Int = T match {
    case E *: _  => 0
    case e *: es => S[IndexOf[es, E]]
  }
}

extension [T <: Tuple] (tuple: T) {
  def applyMappedFunctions[R](functions: Tuple.Map[T, [t] =>> t => R]): Seq[R] = {
    for (i <- 0 until tuple.productArity) yield {
      val elem = tuple.productElement(i).asInstanceOf[Tuple.Union[T]]
      val func = functions.productElement(i).asInstanceOf[Tuple.Union[T] => R]
      func(elem)
    }
  }
  def foldMap[R](functions: Tuple.Map[T, [t] =>> t => R])(using monoid: Monoid[R]): R = {
    monoid.combineAll(applyMappedFunctions(functions))
  }
  def intercalate[R](functions: Tuple.Map[T, [t] =>> t => R], elem: R)(using monoid: Monoid[R]): R = {
    monoid.intercalate(elem).combineAllOption(applyMappedFunctions(functions)).getOrElse(monoid.empty)
  }
}
