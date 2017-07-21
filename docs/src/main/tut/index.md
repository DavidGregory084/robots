---
layout: home

---
## Robots

[![Build Status](https://api.travis-ci.org/DavidGregory084/robots.svg)](https://travis-ci.org/DavidGregory084/robots)
[![Coverage Status](http://codecov.io/github/DavidGregory084/robots/coverage.svg?branch=master)](http://codecov.io/github/DavidGregory084/robots?branch=master)
[![License](https://img.shields.io/github/license/DavidGregory084/robots.svg)](https://opensource.org/licenses/Apache-2.0)

### Overview

Robots is a small Scala library which aims to provide helpful utilities for validating data using the type classes and data types provided by the [Cats](https://github.com/typelevel/cats) library.

### Getting Started

The library is at an early stage of development and no release artifacts have yet been published.

If you want to try out a snapshot you can add the following to your `build.sbt`:

```scala
libraryDependencies += "io.github.davidgregory084" %% "robots" % "0.1.0-SNAPSHOT"
```

### Example

```scala
import robots.Validator, Validator._
import cats.data.{ NonEmptyList, ValidatedNel }
import cats.instances.either._
import cats.instances.list._
import cats.instances.int._

case class Document(maxColumn: Int, maxLines: Int, lines: List[String])

val passing = Document(80, 120, List("Hello", "World"))
val failing = Document(4, 1, List("Hey", "World"))

val documentValidator =
  validate[List, String, Document].
    has(_.maxColumn)(gt(0, List("Max width should be greater than zero"))).
    has(_.maxLines)(gt(0, List("Max line count should be greater than zero"))).
    has2(_.maxLines, _.lines)(Validator {
      case (maxLines, lines) =>
        if (lines.length <= maxLines)
          Nil
        else
          List("Exceeded the maximum number of lines")
    }).
    all2(_.maxColumn, _.lines)(Validator {
      case (maxColumn, line) =>
        if (line.length <= maxColumn)
          Nil
        else
          List("Exceeded the maximum number of columns")
    })

type ValidatedResult[A] = ValidatedNel[String, A]
type EitherNel[A, B] = Either[NonEmptyList[A], B]
type EitherResult[A] = EitherNel[String, A]
```

```scala
documentValidator.run[ValidatedResult](passing)
// res4: ValidatedResult[Document] = Valid(Document(80,120,List(Hello, World)))

documentValidator.run[ValidatedResult](failing)
// res5: ValidatedResult[Document] = Invalid(NonEmptyList(Exceeded the maximum number of lines, Exceeded the maximum number of columns))

documentValidator.run[EitherResult](passing)
// res6: EitherResult[Document] = Right(Document(80,120,List(Hello, World)))

documentValidator.run[EitherResult](failing)
// res7: EitherResult[Document] = Left(NonEmptyList(Exceeded the maximum number of lines, Exceeded the maximum number of columns))
```

### Conduct

Contributors are expected to follow the [Typelevel Code of Conduct](http://typelevel.org/conduct.html) while participating on Github and any other venues associated with the project. 

### Acknowledgements

Thanks to Brendan Maginnis ([@brendanator](https://github.com/brendanator)) for the design which inspired this library.

### License

All code in this repository is licensed under the Apache License, Version 2.0.  See [LICENSE](./LICENSE).

