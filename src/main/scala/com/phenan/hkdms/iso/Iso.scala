package com.phenan.hkdms.iso

case class Iso[A, B](to: A => B, from: B => A)

type <=> [A, B] = Iso[A, B]

given [A, B] (using iso: Iso[A, B]): Iso[B, A] = Iso(iso.from, iso.to)
