package com.phenan.hkdms.example

import com.phenan.hkdms.iso.*
import com.phenan.hkdms.InvariantSemiringal
import com.phenan.hkdms.free.{FreeSRI, Generic}
import com.phenan.hkdms.util.*

case class Printer [T](show: T => String)

object Printer {
  val deriveFromSyntax = [t] => (syntaxElem: SyntaxElem[t]) => {
    syntaxElem match {
      case SyntaxElem.Keyword(keyword) => Printer[t](_ => keyword)
      case SyntaxElem.StringLit        => Printer[t](s => s""""$s"""")
      case SyntaxElem.IntegerLit       => Printer[t](i => i.toString)
    }
  }

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
}
