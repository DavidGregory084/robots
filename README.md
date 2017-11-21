## Robots

[![Build Status](https://api.travis-ci.org/DavidGregory084/robots.svg)](https://travis-ci.org/DavidGregory084/robots)
[![Coverage Status](http://codecov.io/github/DavidGregory084/robots/coverage.svg?branch=master)](http://codecov.io/github/DavidGregory084/robots?branch=master)
[![License](https://img.shields.io/github/license/DavidGregory084/robots.svg)](https://opensource.org/licenses/Apache-2.0)
[![Latest Version](https://img.shields.io/maven-central/v/io.github.davidgregory084/robots-core_2.12.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22io.github.davidgregory084%22%20AND%20a%3A%22robots-core_2.12%22)

### Overview

Robots is a small Scala library which aims to provide helpful utilities for validating data using the type classes and data types provided by the [Cats](https://github.com/typelevel/cats) library.

### Getting Started

Add the following to your `build.sbt`:

```scala
libraryDependencies += "io.github.davidgregory084" %% "robots" % "0.1.0-RC1"
```

### Example

```scala
import robots.Validator, Validator._
import cats.data.Validated
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
```

```scala
// Works well with `Either` and `Validated`
documentValidator.runNel[Validated](passing)
// res4: cats.data.Validated[cats.data.NonEmptyList[String],Document] = Valid(Document(80,120,List(Hello, World)))

documentValidator.runNel[Validated](failing)
// res5: cats.data.Validated[cats.data.NonEmptyList[String],Document] = Invalid(NonEmptyList(Exceeded the maximum number of lines, Exceeded the maximum number of columns))

documentValidator.runNel[Either](passing)
// res6: Either[cats.data.NonEmptyList[String],Document] = Right(Document(80,120,List(Hello, World)))

documentValidator.runNel[Either](failing)
// res7: Either[cats.data.NonEmptyList[String],Document] = Left(NonEmptyList(Exceeded the maximum number of lines, Exceeded the maximum number of columns))

// You can brjng any `MonadError`
import cats.data.Ior
// import cats.data.Ior

documentValidator.run[Ior[List[String], ?]](passing)
// res9: cats.data.Ior[List[String],Document] = Right(Document(80,120,List(Hello, World)))

documentValidator.run[Ior[List[String], ?]](failing)
// res10: cats.data.Ior[List[String],Document] = Left(List(Exceeded the maximum number of lines, Exceeded the maximum number of columns))

// Even ones that discard errors
import cats.instances.option._
// import cats.instances.option._

documentValidator.run_[Option](passing)
// res12: Option[Document] = Some(Document(80,120,List(Hello, World)))

documentValidator.run_[Option](failing)
// res13: Option[Document] = None
```

### Conduct

Contributors are expected to follow the [Typelevel Code of Conduct](http://typelevel.org/conduct.html) while participating on Github and any other venues associated with the project. 

### Acknowledgements

Thanks to Brendan Maginnis ([@brendanator](https://github.com/brendanator)) for the design which inspired this library.

### License

All code in this repository is licensed under the Apache License, Version 2.0.  See [LICENSE](./LICENSE).
