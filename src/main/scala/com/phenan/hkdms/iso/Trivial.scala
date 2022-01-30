package com.phenan.hkdms.iso

given [A]: Iso[A, (A, Unit)] = Iso((_, ()), _._1)
given [A]: Iso[A, (Unit, A)] = Iso(((), _), _._2)
