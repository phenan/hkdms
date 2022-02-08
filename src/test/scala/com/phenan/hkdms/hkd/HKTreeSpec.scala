package com.phenan.hkdms.hkd

import org.scalatest.funsuite.AnyFunSuite

case class UserProfile(name: String, age: Int)

class HKTreeSpec extends AnyFunSuite {
  test("単純なデータ型 / 名前指定で生成 / フィールドは全てSome") {
    val profile: HKStruct[UserProfile, Option] = HKStruct[UserProfile, Option](
      name = Some("UserName"),
      age  = Some(20)
    )
    assert(profile.fold == Some(UserProfile(name = "UserName", age = 20)))
  }

  test("単純なデータ型 / 名前指定で生成 / Noneフィールドあり") {
    val profile: HKStruct[UserProfile, Option] = HKStruct[UserProfile, Option](
      name = None,
      age  = Some(20)
    )
    assert(profile.fold == None)
  }

  test("単純なデータ型 / 名前指定なし / フィールドは全てSome") {
    val profile: HKStruct[UserProfile, Option] = HKStruct[UserProfile, Option](
      Some("UserName"),
      Some(20)
    )
    assert(profile.fold == Some(UserProfile(name = "UserName", age = 20)))
  }
}
