package com.phenan.hkdms

import cats.{Alternative, InvariantMonoidal}
import com.phenan.hkdms.util.{IndexedUnion, *}

trait InvariantSemiringal [F[_]] extends InvariantMonoidal[F] {
  def pure [A] (a: => A): F[A]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def sum [A, B](fa: F[A], fb: F[B]): F[Either[A, B]]

  def imap[A, B] (fa: F[A])(f: A => B)(g: B => A): F[B]

  override def unit: F[Unit] = pure(())

  // NOTE: T <: NonEmptyTuple としたいが Mirror.SumOf の MirroredElemTypes が non empty ではないのでできない。
  // 従って最も正しいのは加法単位元の導入だが、それをしてしまうと IndexedUnion を複雑化した上で Either などが InvariantSemiringal にならなくなる。
  // 典型的な利用方法では EmptyTuple をこいつに入れることはないはずなので、一旦はこの実装で良いこととする
  @annotation.nowarn
  private[hkdms] def sumAll [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[IndexedUnion[T]] = tupleMap match {
    case (fe: F[h]) *: EmptyTuple =>
      imap(fe) { e => IndexedUnion[T](e.asInstanceOf[Tuple.Union[T]], 0) } { _.value.asInstanceOf[h] }
    case (f1: F[h1]) *: (f2: F[h2]) *: (rest: Tuple.Map[t, F]) =>
      imap(sum(f1, sumAll[h2 *: t](f2 *: rest))) {
        case Left(e)  => IndexedUnion[T](e.asInstanceOf[Tuple.Union[T]], 0)
        case Right(u) => IndexedUnion[T](u.value.asInstanceOf[Tuple.Union[T]], u.index + 1)
      } { union =>
        if (union.index == 0) {
          Left(union.value.asInstanceOf[h1])
        } else {
          Right(IndexedUnion[h2 *: t](union.value.asInstanceOf[Tuple.Union[h2 *: t]], union.index - 1))
        }
      }
  }
}

object InvariantSemiringal {
  given [F[_]] (using alternative: Alternative[F]): InvariantSemiringal[F] with {
    override def pure[A](a: => A): F[A] = alternative.pure(a)
    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = alternative.product(fa, fb)
    override def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]] = alternative.sum(fa, fb)
    override def imap[A, B] (fa: F[A])(f: A => B)(g: B => A): F[B] = alternative.imap(fa)(f)(g)
  }
}
