package com.phenan.hkdms.free

import com.phenan.hkdms.iso.*

import scala.compiletime.erasedValue
import scala.deriving.*
import scala.util.NotGiven

trait Generic [T] {

}

object Generic {
  case class Leaf [T] () extends Generic[T]

  inline given [P <: Product](using mirror: Mirror.ProductOf[P], fields: ElemGenerics[mirror.MirroredElemTypes], iso: mirror.MirroredElemTypes <=> P): FreeSRI[Generic, P] = {
    FreeSRI.Product(fields.generics).iMap(iso)
  }

  inline given [U](using mirror: Mirror.SumOf[U], elems: ElemGenerics[mirror.MirroredElemTypes], iso: Tuple.Union[mirror.MirroredElemTypes] <=> U): FreeSRI[Generic, U] = {
    FreeSRI.Union(elems.generics).iMap(iso)
  }

  inline given ElemGenerics[EmptyTuple] = new ElemGenerics(EmptyTuple)
  inline given [S, T <: Tuple](using fields: ElemGenerics[T], generic: FreeSRI[Generic, S]): ElemGenerics[S *: T] = {
    generic *: fields
  }

  inline given [S, T <: Tuple, L <: String, Labels <: Tuple](using fields: ElemGenerics[T], generic: NotGiven[FreeSRI[Generic, S]]): ElemGenerics[S *: T] = {
    (FreeSRI.Impure(Leaf[S]())) *: fields
  }

  class ElemGenerics[T <: Tuple](val generics: Tuple.Map[T, [f] =>> FreeSRI[Generic, f]]) {
    def *: [S, L] (v: FreeSRI[Generic, S]): ElemGenerics[S *: T] = new ElemGenerics[S *: T](v *: generics)
  }
}
