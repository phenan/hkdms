package com.phenan.hkdms.util

import scala.compiletime.ops.int.S

object Tuples {
  type IndexOf[T <: Tuple, E] <: Int = T match {
    case E *: _  => 0
    case e *: es => S[IndexOf[es, E]]
  }
}
