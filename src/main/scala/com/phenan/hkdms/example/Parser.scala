package com.phenan.hkdms.example

import cats.data.StateT

sealed trait Token
object Token {
  case class StrLit (string: String) extends Token
  case class IntLit (integer: Int) extends Token
  case class Keyword (keyword: String) extends Token
}

type Parser[T] = StateT[Option, List[Token], T]

object Parser {
  val deriveFromSyntax = [t] => (syntaxElem: SyntaxElem[t]) => {
    syntaxElem match {
      case SyntaxElem.Keyword(keyword) => keywordParser(keyword)
      case SyntaxElem.StringLit        => stringLitParser
      case SyntaxElem.IntegerLit       => integerLitParser
    }
  }.asInstanceOf[Parser[t]]

  def keywordParser(keyword: String): Parser[Unit] = StateT { (in: List[Token]) =>
    if (in.headOption.exists(_ == Token.Keyword(keyword))) {
      Some((in.tail, ()))
    } else {
      None
    }
  }

  val stringLitParser: Parser[String] = StateT { (in: List[Token]) =>
    in match {
      case Token.StrLit(str) :: rest => Some((rest, str))
      case _ => None
    }
  }

  val integerLitParser: Parser[Int] = StateT { (in: List[Token]) =>
    in match {
      case Token.IntLit(i) :: rest => Some((rest, i))
      case _ => None
    }
  }
}
