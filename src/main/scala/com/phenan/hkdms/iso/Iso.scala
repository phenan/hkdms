package com.phenan.hkdms.iso

case class Iso[A, B](to: A => B, from: B => A)

type <=> [A, B] = Iso[A, B]

given [A, B] (using iso: Iso[A, B]): Iso[B, A] = Iso(iso.from, iso.to)
given [A]: Iso[A, (A, Unit)] = Iso((_, ()), _._1)
given [A]: Iso[A, (Unit, A)] = Iso(((), _), _._2)
