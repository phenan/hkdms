package com.phenan.hkdms

import cats.Defer
import com.phenan.hkdms.InvariantSemiringal
import com.phenan.hkdms.util.*
import org.scalatest.funsuite.AnyFunSuite

class HKForestSpec extends AnyFunSuite {
  case class UserGroup (users: List[User])
  sealed trait User
  case class GuestUser (guestId: Int) extends User
  case class RegisteredUser (userId: Int, profile: UserProfile) extends User
  case class UserProfile (name: String, age: Int)

  case class Printer [T](show: T => String)

  object Printer {
    val int: HKForest[Int, Printer] = HKValue(Printer(_.toString))
    val string: HKForest[String, Printer] = HKValue(Printer(identity))

    def word(string: String): HKForest[Unit, Printer] = HKValue(Printer(_ => string))
  }

  given InvariantSemiringal[Printer] with {
    override def pure[A](a: => A): Printer[A] = {
      Printer[A] (value => "")
    }

    override def product[A, B](fa: Printer[A], fb: Printer[B]): Printer[(A, B)] = {
      Printer[(A, B)] ((a, b) => fa.show(a) ++ fb.show(b))
    }

    override def sum[A, B](fa: Printer[A], fb: Printer[B]): Printer[Either[A, B]] = {
      Printer[Either[A, B]] (_.fold(fa.show, fb.show))
    }

    override def imap[A, B](printer: Printer[A])(f: A => B)(g: B => A): Printer[B] = {
      Printer[B] (value => printer.show(g(value)))
    }
  }

  given Defer[Printer] with {
    override def defer[A](fa: => Printer[A]): Printer[A] = {
      Printer[A] (value => fa.show(value))
    }
  }

  private def showFunctionMap[T <: Tuple](tupleMap: Tuple.Map[T, Printer]): Tuple.Map[T, [t] =>> t => String] = {
    TupleMaps.map[T, Printer, [t] =>> t => String](tupleMap) { [t] => (show: Printer[t]) => (value: t) => show.show(value) }
  }

  test("Printer / 単純なデータ型 / 名前指定") {
    val profile = HKProduct[UserProfile, Printer] (
      name = Printer.string,
      age = Printer.int
    )
    val printer = profile.fold
    assert(printer.show(UserProfile(name = "Name", age = 21)) == "Name21")
  }

  test("Printer / 直和型 / 名前指定") {
    import Printer._

    def wrap[T](name: String)(printer: HKForest[T, Printer]): HKForest[T, Printer] = {
      word(s"$name(") *>: printer :<* word(")")
    }

    val user = HKSum[User, Printer](
      wrap("GuestUser") {
        HKProduct[GuestUser, Printer](
          guestId = word("guest id: ") *>: int
        )
      },
      wrap("RegisteredUser") {
        HKProduct[RegisteredUser, Printer](
          userId = word("user id: ") *>: int :<* word(", "),
          profile = wrap("UserProfile") {
            HKProduct[UserProfile, Printer](
              name = word("name: ") *>: string :<* word(", "),
              age = word("age: ") *>: int
            )
          }
        )
      }
    )

    val printer = user.fold
    assert(printer.show(RegisteredUser(userId = 1234, profile = UserProfile(name = "user name", age = 35))) == "RegisteredUser(user id: 1234, UserProfile(name: user name, age: 35))")
  }

  test("List") {
    import Printer._

    def wrap[T](name: String)(printer: HKForest[T, Printer]): HKForest[T, Printer] = {
      word(s"$name(") *>: printer :<* word(")")
    }

    val user = HKSum[User, Printer](
      wrap("GuestUser") {
        HKProduct[GuestUser, Printer](
          guestId = word("guest id: ") *>: int
        )
      },
      wrap("RegisteredUser") {
        HKProduct[RegisteredUser, Printer](
          userId = word("user id: ") *>: int :<* word(", "),
          profile = wrap("UserProfile") {
            HKProduct[UserProfile, Printer](
              name = word("name: ") *>: string :<* word(", "),
              age = word("age: ") *>: int
            )
          }
        )
      }
    )

    val userGroup = wrap("UserGroup") {
      HKProduct[UserGroup, Printer](user.rep0)
    }

    val printer = userGroup.fold

    val result = printer.show(UserGroup(List(RegisteredUser(userId = 1234, profile = UserProfile(name = "user name", age = 35)))))

    assert(result == "UserGroup(RegisteredUser(user id: 1234, UserProfile(name: user name, age: 35)))")
  }
}
