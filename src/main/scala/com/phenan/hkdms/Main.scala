package com.phenan.hkdms

import cats.Id
import com.phenan.hkdms.free.*
import com.phenan.hkdms.hkd.{HKValue, *}
import com.phenan.hkdms.iso.Iso
import com.phenan.hkdms.iso
import com.phenan.hkdms.example.*

object Main {
  def main(args: Array[String]): Unit = {
    val hkd1: HKD[Foo, Id] = HKD[Foo, Id](1, "one")

    val i: Int = hkd1.i
    val s: String =  hkd1.s

    println(i)
    println(s)

    val hkd2 = HKD[Foo, Option](Some(1), Some("string"))

    val i2: Option[Int] = hkd2.i
    val s2: Option[String] = hkd2.s

    println(i2)
    println(s2)

    val hkd3 = hkd1.map([t] => (fa: Id[t]) => Right(fa): Either[Nothing, t])
    val i3: Either[Nothing, Int] = hkd3.i
    val s3: Either[Nothing, String] = hkd3.s

    println(i3)
    println(s3)

    val printer: Printer[Hoge] = hogeSyntax.foldMap[Printer](Printer.deriveFromSyntax)

    println(printer.show(Foo(10, "hoge")))

    val parser: Parser[Hoge] = hogeSyntax.foldMap[Parser](Parser.deriveFromSyntax)
    val result: Option[Hoge] = parser.runA(List(Token.Keyword("x"), Token.StrLit("hogehoge")))
    println(result)

    println(Some(Bar("hogehoge")) == result)

    val hktree: HKTree[Hoge, Option] = HKStruct[Foo, Option](
      i = HKValue(Option(10)),
      s = HKValue(Option("str"))
    ).widen[Hoge]

    val hogeOpt: Option[Hoge] = hktree.fold

    println(hogeOpt)
  }
}
