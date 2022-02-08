package com.phenan.hkdms

import cats.instances.list.*
import com.phenan.hkdms.free.*
import com.phenan.hkdms.hkd.*
import com.phenan.hkdms.example.*

object Main {
  def main(args: Array[String]): Unit = {
    val hkd1: HKD[Foo, Option] = HKD[Foo, Option](Some(1), Some("one"))

    val i: Option[Int] = hkd1.i
    val s: Option[String] =  hkd1.s

    println(i)
    println(s)

    val hkd2 = HKD[Bar, Option](Some("string"))

    val x: Option[String] = hkd2.x

    println(x)

    val hkd3 = hkd1.map([t] => (fa: Option[t]) => fa.toRight("fail"))
    val i3: Either[String, Int] = hkd3.i
    val s3: Either[String, String] = hkd3.s

    println(i3)
    println(s3)

    val printer: Printer[Hoge] = hogeSyntax.foldMap[Printer](Printer.deriveFromSyntax)

    println(printer.show(Foo(10, "hoge")))

    val parser: Parser[Hoge] = hogeSyntax.foldMap[Parser](Parser.deriveFromSyntax)
    val result: Option[Hoge] = parser.runA(List(Token.Keyword("x"), Token.StrLit("hogehoge")))
    println(result)

    println(Some(Bar("hogehoge")) == result)

    val hktree: HKTree[Hoge, Option] = HKStruct[Foo, Option](
      i = Option(10),
      s = Option("str")
    ).widen[Hoge]

    val hogeOpt: Option[Hoge] = hktree.fold

    println(hogeOpt)

    val piyoTree = HKStruct[Piyo, Option](
      HKList(List)(
        HKStruct[Foo, Option](
          Option(1),
          Option("one")
        ).widen[Hoge],
        HKStruct[Bar, Option](
          Option("bar")
        ).widen[Hoge]
      )
    )

    println(piyoTree.fold)
  }
}
