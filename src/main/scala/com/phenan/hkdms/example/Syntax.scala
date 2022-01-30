package com.phenan.hkdms.example

import com.phenan.hkdms.free.FreeSRI
import com.phenan.hkdms.iso.given

sealed trait SyntaxElem [T]

object SyntaxElem {
  case class Keyword (value: String) extends SyntaxElem[Unit]
  case object StringLit extends SyntaxElem[String]
  case object IntegerLit extends SyntaxElem[Int]
}

type Syntax[T] = FreeSRI[SyntaxElem, T]

object Syntax {
  def keyword(str: String) = FreeSRI.Impure(SyntaxElem.Keyword(str))
  val stringLit = FreeSRI.Impure(SyntaxElem.StringLit)
  val integerLit = FreeSRI.Impure(SyntaxElem.IntegerLit)
}

sealed trait Hoge
case class Foo(i: Int, s: String) extends Hoge
case class Bar(x: String) extends Hoge

val fooSyntax: Syntax[Foo] = FreeSRI.struct[Foo] {
  (Syntax.keyword("i") *>: Syntax.integerLit)
    *: (Syntax.keyword("s") *>: Syntax.stringLit)
    *: FreeSRI.nil
}

val barSyntax: Syntax[Bar] = FreeSRI.struct[Bar] {
  (Syntax.keyword("x") *>: Syntax.stringLit)
    *: FreeSRI.nil
}

val hogeSyntax: Syntax[Hoge] = FreeSRI.union[Hoge] {
  fooSyntax
    *: barSyntax
    *: FreeSRI.nil
}
