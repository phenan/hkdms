package com.phenan.hkdms.util

object TupleMaps {
  type Unwrap[F[_]] = [T] =>> T match {
    case F[t] => t
  }

  def map[T <: Tuple, F[_], G[_]](tuple: Tuple.Map[T, F])(f: [a] => F[a] => G[a]): Tuple.Map[T, G] = {
    tuple.map [[fa] =>> G[Unwrap[F][fa]]] { [fa] => (value: fa) =>
      f[Unwrap[F][fa]](value.asInstanceOf[F[Unwrap[F][fa]]])
    }.asInstanceOf[Tuple.Map[T, G]]
  }
}
