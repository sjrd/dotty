---
layout: doc-page
title: "Union Types - More Details"
nightlyOf: https://docs.scala-lang.org/scala3/reference/new-types/union-types-spec.html
---

## Type inference

When inferring the result type of a definition (`val`, `var`, or `def`) and the
type we are about to infer is a union type, then we replace it by its join.
Similarly, when instantiating a type argument, if the corresponding type
parameter is not upper-bounded by a union type and the type we are about to
instantiate is a union type, we replace it by its join. This mirrors the
treatment of singleton types which are also widened to their underlying type
unless explicitly specified. The motivation is the same: inferring types
which are "too precise" can lead to unintuitive typechecking issues later on.

**Note:** Since this behavior limits the usability of union types, it might
be changed in the future. For example by not widening unions that have been
explicitly written down by the user and not inferred, or by not widening a type
argument when the corresponding type parameter is covariant.

See [PR #2330](https://github.com/lampepfl/dotty/pull/2330) and
[Issue #4867](https://github.com/lampepfl/dotty/issues/4867) for further discussions.

### Example

```scala
import scala.collection.mutable.ListBuffer
val x = ListBuffer(Right("foo"), Left(0))
val y: ListBuffer[Either[Int, String]] = x
```

This code typechecks because the inferred type argument to `ListBuffer` in the
right-hand side of `x` was `Left[Int, Nothing] | Right[Nothing, String]` which
was widened to `Either[Int, String]`. If the compiler hadn't done this widening,
the last line wouldn't typecheck because `ListBuffer` is invariant in its
argument.
