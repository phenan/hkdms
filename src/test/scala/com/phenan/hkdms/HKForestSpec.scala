package com.phenan.hkdms

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

  given InvariantSemiringal[Printer] = new InvariantSemiringal[Printer] {
    override def pure[A](a: => A): Printer[A] = {
      Printer[A] (value => "")
    }

    override def productAll[T <: Tuple](tupleMap: Tuple.Map[T, Printer]): Printer[T] = {
      Printer[T] (_.intercalate(showFunctionMap(tupleMap), " "))
    }

    override def sumAll[T <: Tuple](tupleMap: Tuple.Map[T, Printer]): Printer[IndexedUnion[T]] = {
      Printer[IndexedUnion[T]] (_.fold(showFunctionMap(tupleMap)))
    }

    override def imap[A, B](printer: Printer[A])(f: A => B)(g: B => A): Printer[B] = {
      Printer[B] (value => printer.show(g(value)))
    }
  }

  private def showFunctionMap[T <: Tuple](tupleMap: Tuple.Map[T, Printer]): Tuple.Map[T, [t] =>> t => String] = {
    TupleMaps.map[T, Printer, [t] =>> t => String](tupleMap) { [t] => (show: Printer[t]) => (value: t) => show.show(value) }
  }

  test("Printer / 単純なデータ型 / 名前指定") {
    val profile = HKProduct[UserProfile, Printer] (
      name = Printer(identity),
      age = Printer(_.toString)
    )
    val printer = profile.fold
    assert(printer.show(UserProfile(name = "Name", age = 21)) == "Name 21")
  }
}
