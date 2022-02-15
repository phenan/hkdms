package com.phenan.hkdms.util

import com.phenan.hkdms.InvariantSemiringal

extension [F[_]] (invariantSemiringal: InvariantSemiringal[F]) {
  // NOTE: T <: NonEmptyTuple としたいが Mirror.SumOf の MirroredElemTypes が non empty ではないのでできない。
  // 従って最も正しいのは加法単位元の導入だが、それをしてしまうと IndexedUnion を複雑化した上で Either などが InvariantSemiringal にならなくなる。
  // 典型的な利用方法では EmptyTuple をこいつに入れることはないはずなので、一旦はこの実装で良いこととする
  @annotation.nowarn
  private[hkdms] def sumAll [T <: Tuple] (tupleMap: Tuple.Map[T, F]): F[IndexedUnion[T]] = tupleMap match {
    case (fe: F[h]) *: EmptyTuple =>
      invariantSemiringal.imap(fe) { e => IndexedUnion[T](e.asInstanceOf[Tuple.Union[T]], 0) } { _.value.asInstanceOf[h] }
    case (f1: F[h1]) *: (f2: F[h2]) *: (rest: Tuple.Map[t, F]) =>
      invariantSemiringal.imap(invariantSemiringal.sum(f1, sumAll[h2 *: t](f2 *: rest))) {
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
