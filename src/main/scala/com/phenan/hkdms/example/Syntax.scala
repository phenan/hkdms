package com.phenan.hkdms.example

import com.phenan.hkdms.free.FreeSRI
import com.phenan.hkdms.free.syntax.FreeSRISyntax.*
import com.phenan.hkdms.iso.given

sealed trait SyntaxElem [T]

object SyntaxElem {
  case class Keyword (value: String) extends SyntaxElem[Unit]
  case object StringLit extends SyntaxElem[String]
  case object IntegerLit extends SyntaxElem[Int]
}

type Syntax[T] = FreeSRI[SyntaxElem, T]

object Syntax {
  def keyword(str: String) = impure(SyntaxElem.Keyword(str))
  val stringLit = impure(SyntaxElem.StringLit)
  val integerLit = impure(SyntaxElem.IntegerLit)
}

sealed trait Hoge
case class Foo(i: Int, s: String) extends Hoge
case class Bar(x: String) extends Hoge

val fooSyntax: Syntax[Foo] = struct[Foo] {
  (Syntax.keyword("i") *>: Syntax.integerLit)
    *: (Syntax.keyword("s") *>: Syntax.stringLit)
    *: nil
}

val barSyntax: Syntax[Bar] = struct[Bar] {
  (Syntax.keyword("x") *>: Syntax.stringLit)
    *: nil
}

val hogeSyntax: Syntax[Hoge] = union[Hoge] {
  fooSyntax
    *: barSyntax
    *: nil
}
