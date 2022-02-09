package com.phenan.hkdms.hkd

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class HKDSpec extends AnyFunSuite {
  case class TestScore(score: Int)
  case class UserProfile(name: String, age: Int)

  test("単独フィールド / 名前指定で生成 / フィールドはSome") {
    val hkd = HKD[TestScore, Option](
      score = Some(60)
    )
    assert(hkd.score == Some(60))
    assert(hkd.fold == Some(TestScore(60)))
  }

  test("単独フィールド / 名前指定なし / フィールドはSome") {
    val hkd = HKD[TestScore, Option](
      Some(60)
    )
    assert(hkd.score == Some(60))
    assert(hkd.fold == Some(TestScore(60)))
  }

  test("単独フィールド / 名前指定で生成 / フィールドはNone") {
    val hkd = HKD[TestScore, Option](
      score = None
    )
    assert(hkd.score == None)
    assert(hkd.fold == None)
  }

  test("単独フィールド / 名前指定なし / フィールドはNone") {
    val hkd = HKD[TestScore, Option](
      None
    )
    assert(hkd.score == None)
    assert(hkd.fold == None)
  }

  test("複数フィールド / 名前指定で生成 / フィールドは全てSome") {
    val hkd = HKD[UserProfile, Option](
      name = Some("name"),
      age = Some(20)
    )
    assert(hkd.name == Some("name"))
    assert(hkd.age == Some(20))
    assert(hkd.fold == Some(UserProfile("name", 20)))
  }

  test("複数フィールド / 名前指定なし / フィールドは全てSome") {
    val hkd = HKD[UserProfile, Option](Some("name"), Some(20))
    assert(hkd.name == Some("name"))
    assert(hkd.age == Some(20))
    assert(hkd.fold == Some(UserProfile("name", 20)))
  }

  test("複数フィールド / 名前指定で生成 / フィールドにNoneを含む") {
    val hkd = HKD[UserProfile, Option](
      name = None,
      age = Some(20)
    )
    assert(hkd.name == None)
    assert(hkd.age == Some(20))
    assert(hkd.fold == None)
  }

  test("複数フィールド / 名前指定なし / フィールドにNoneを含む") {
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
