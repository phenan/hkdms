package com.phenan.hkdms.hkd

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class HKDSpec extends AnyFunSuite {
  case class UserProfile(name: String, age: Int)

  test("名前指定で生成 / フィールドは全てSome") {
    val hkd = HKD[UserProfile, Option](
      name = Some("name"),
      age = Some(20)
    )
    assert(hkd.name == Some("name"))
    assert(hkd.age == Some(20))
    assert(hkd.fold == Some(UserProfile("name", 20)))
  }

  test("名前指定なし / フィールドは全てSome") {
    val hkd = HKD[UserProfile, Option](Some("name"), Some(20))
    assert(hkd.name == Some("name"))
    assert(hkd.age == Some(20))
    assert(hkd.fold == Some(UserProfile("name", 20)))
  }

  test("名前指定で生成 / フィールドにNoneを含む") {
    val hkd = HKD[UserProfile, Option](
      name = None,
      age = Some(20)
    )
    assert(hkd.name == None)
    assert(hkd.age == Some(20))
    assert(hkd.fold == None)
  }

  test("名前指定なし / フィールドにNoneを含む") {
    val hkd = HKD[UserProfile, Option](Some("name"), None)
    assert(hkd.name == Some("name"))
    assert(hkd.age == None)
    assert(hkd.fold == None)
  }

  test("名前間違いでコンパイルエラー") {
    """HKD[UserProfile, Option](name = Some("name"), id = Some(20))""" shouldNot compile
  }

  test("名前は正しいが型が間違っていてコンパイルエラー") {
    """HKD[UserProfile, Option](name = Some("name"), age = Some(20.0))""" shouldNot compile
  }

  test("ラップする型が間違っていてコンパイルエラー") {
    """HKD[UserProfile, Option](name = Right("name"), age = Right(20))""" shouldNot compile
  }

  test("余計な引数がついていてコンパイルエラー") {
    """HKD[UserProfile, Option](name = Some("name"), age = Some(20), id = Some(100))""" shouldNot compile
  }

  test("名前なしで型が間違っていてコンパイルエラー") {
    """HKD[UserProfile, Option](Some("name"), Some(20.0))""" shouldNot compile
  }
}
