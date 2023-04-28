---
layout: doc-page
title: "Intersection Types - More Details"
nightlyOf: https://docs.scala-lang.org/scala3/reference/new-types/intersection-types-spec.html
---

## Relationship with Compound Type (`with`)

Intersection types `A & B` replace compound types `A with B` in Scala 2. For the
moment, the syntax `A with B` is still allowed and interpreted as `A & B`, but
its usage as a type (as opposed to in a `new` or `extends` clause) will be
deprecated and removed in the future.
