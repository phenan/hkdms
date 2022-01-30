package com.phenan.hkdms

import com.phenan.hkdms.data.*
import com.phenan.hkdms.hkd.HKD
import com.phenan.hkdms.iso.Iso
import com.phenan.hkdms.iso.given

import com.phenan.hkdms.example.hogeGeneric

case class Foo(i: Int, s: String)
case class Bar(foo: Foo)

object Main {
  def main(args: Array[String]): Unit = {
    val hkd1: HKD[Foo, Identity] = HKD.fromProduct(Foo(1, "one"))

    val i: Int = hkd1.i
    val s: String =  hkd1.s

    println(i)
    println(s)

    val tuple: Tuple.Map[(Int, String), Option] = (Some(1), Some("string"))
    val hkd2 = HKD.fromTuple[Foo, Option](tuple)

    val i2: Option[Int] = hkd2.i
    val s2: Option[String] = hkd2.s

    println(i2)
    println(s2)

    val hkd3 = hkd1.map([t] => (fa: Identity[t]) => Right(fa): Either[Nothing, t])
    val i3: Either[Nothing, Int] = hkd3.i
    val s3: Either[Nothing, String] = hkd3.s

    println(i3)
    println(s3)

    val iso = summon[Iso[Foo, (Int, String)]]

    println(hogeGeneric)
  }
}
