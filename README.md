# hkdms: Higher-Kinded Data by Mirrors in Scala3

[![Scala CI](https://github.com/phenan/hkdms/actions/workflows/scala.yml/badge.svg)](https://github.com/phenan/hkdms/actions/workflows/scala.yml)

## Type hierarchy

### HKD

* `HKD[T, F]`

### HKTree

* `HKStruct[T, F]`
* `HKList[C, T, F]`

### HKForest

* `HKProduct[T, F]`
* `HKSum[T, F]` 
* `HKFix[T, F]`
* `HKValue[T, F]`
