package com.phenan.hkdms

import cats.{Alternative, InvariantMonoidal}
import com.phenan.hkdms.util.{IndexedUnion, *}

trait InvariantSemiringal [F[_]] extends InvariantMonoidal[F] {
  def pure [A] (a: => A): F[A]

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def sum [A, B](fa: F[A], fb: F[B]): F[Either[A, B]]

  def imap[A, B] (fa: F[A])(f: A => B)(g: B => A): F[B]

  override def unit: F[Unit] = pure(())
}

object InvariantSemiringal {
  given [F[_]] (using alternative: Alternative[F]): InvariantSemiringal[F] with {
    override def pure[A](a: => A): F[A] = alternative.pure(a)
    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = alternative.product(fa, fb)
    override def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]] = alternative.sum(fa, fb)
    override def imap[A, B] (fa: F[A])(f: A => B)(g: B => A): F[B] = alternative.imap(fa)(f)(g)
  }
}
