package com.phenan.hkdms

import cats.data.{NonEmptyChain, Validated}
import org.scalatest.funsuite.AnyFunSuite

class HKTreeSpec extends AnyFunSuite {
  case class UserGroup (users: List[User])
  sealed trait User
  case class GuestUser (guestId: Int) extends User
  case class RegisteredUser (userId: Int, profile: UserProfile) extends User
  case class UserProfile (name: String, age: Int)

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
  
  test("ネストしたデータ型 / 名前指定で生成 / フィールドは全てSome") {
    val registeredUser = HKStruct[RegisteredUser, Option](
      userId = Some(1234),
      profile = HKStruct[UserProfile, Option](
        name = Some("user name"),
        age = Some(35)
      )
    )
    assert(registeredUser.fold == Some(RegisteredUser(userId = 1234, profile = UserProfile(name = "user name", age = 35))))
  }

  test("ネストしたデータ型 / 名前指定で生成 / None フィールドあり") {
    val registeredUser = HKStruct[RegisteredUser, Option](
      userId = Some(1234),
      profile = HKStruct[UserProfile, Option](
        name = Some("user name"),
        age = None
      )
    )
    assert(registeredUser.fold == None)
  }

  test("ネストしたデータ型 / 名前指定なし / フィールドは全てSome") {
    val registeredUser = HKStruct[RegisteredUser, Option](
      Some(1234),
      HKStruct[UserProfile, Option](
        Some("user name"),
        Some(35)
      )
    )
    assert(registeredUser.fold == Some(RegisteredUser(userId = 1234, profile = UserProfile(name = "user name", age = 35))))
  }

  test("サブタイピング") {
    val user: HKTree[User, Option] = HKStruct[RegisteredUser, Option](
      Some(1234),
      HKStruct[UserProfile, Option](
        Some("user name"),
        Some(35)
      )
    )
    assert(user.fold == Some(RegisteredUser(userId = 1234, profile = UserProfile(name = "user name", age = 35))))
  }

  test("リスト") {
    val userGroup = HKStruct[UserGroup, Option](
      HKList(List)(
        HKStruct[RegisteredUser, Option](
          userId = Some(1234),
          profile = HKStruct[UserProfile, Option](
            name = Some("user name"),
            age = Some(35)
          )
        ),
        HKStruct[GuestUser, Option](
          guestId = Some(5678)
        )
      )
    )
    assert(userGroup.fold == Some(UserGroup(List(RegisteredUser(userId = 1234, profile = UserProfile(name = "user name", age = 35)), GuestUser(guestId = 5678)))))
  }

  test("Eitherで構築しておいてValidatedNecで取り出す") {
    type Checked[T] = Either[String, T]
    val checkedUser: HKTree[User, Checked] = HKStruct[RegisteredUser, Checked](
      userId = Left("user id should be positive"),
      profile = HKStruct[UserProfile, Checked](
        name = Right("name"),
        age = Left("age should be smaller than 1000")
      )
    )
    assert(checkedUser.fold == Left("user id should be positive"))

    val validated = checkedUser.foldMap { [t] => (checked: Checked[t]) => Validated.fromEither(checked).toValidatedNec }

    assert(validated == Validated.Invalid(NonEmptyChain("user id should be positive", "age should be smaller than 1000")))
  }
}
