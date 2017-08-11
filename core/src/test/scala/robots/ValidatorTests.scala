package robots

import cats.{ ApplicativeError, Eq, MonoidK, Traverse }
import cats.data.{ NonEmptyList, Validated }
import cats.laws.discipline.eq._
import cats.tests.CatsSuite
import org.scalacheck.{ Arbitrary, Cogen }

class ValidatorTests extends CatsSuite {

  implicit def arbPValidator[F[_]: Traverse, E, A, B](implicit M: MonoidK[F], CA: Cogen[A], E: Arbitrary[F[E]], B: Arbitrary[B]): Arbitrary[PValidator[F, E, A, B]] =
    Arbitrary {
      for {
        f <- Arbitrary.arbitrary[A => F[E]]
        g <- Arbitrary.arbitrary[A => B]
      } yield PValidator(f, g)
    }

  implicit def eqPValidator[F[_]: Traverse, G[_, _], E, A, B](implicit AE: ApplicativeError[G[NonEmptyList[E], ?], NonEmptyList[E]], A: Arbitrary[A], E: Eq[G[NonEmptyList[E], B]]): Eq[PValidator[F, E, A, B]] =
    Eq.by[PValidator[F, E, A, B], A => G[NonEmptyList[E], B]](_.run[G])

  test("Validate using eql") {
    val eqlOne1 = Validator.eql(1, (i: Int) => Option(s"$i was not equal to 1"))

    eqlOne1.run[Validated](1) should ===(Validated.valid(1))
    eqlOne1.run[Validated](2) should ===(Validated.invalid(NonEmptyList.of(s"2 was not equal to 1")))

    val eqlOne2 = Validator.eql(1, Option("Was not equal to 1"))

    eqlOne2.run[Validated](1) should ===(Validated.valid(1))
    eqlOne2.run[Validated](2) should ===(Validated.invalid(NonEmptyList.of("Was not equal to 1")))
  }

  test("Validate using neq") {
    val neqOne1 = Validator.neq(1, (i: Int) => Option(s"$i was equal to 1"))

    neqOne1.run[Validated](2) should ===(Validated.valid(2))
    neqOne1.run[Validated](1) should ===(Validated.invalid(NonEmptyList.of(s"1 was equal to 1")))

    val neqOne2 = Validator.neq(1, Option("Was equal to 1"))

    neqOne2.run[Validated](2) should ===(Validated.valid(2))
    neqOne2.run[Validated](1) should ===(Validated.invalid(NonEmptyList.of("Was equal to 1")))
  }

  test("Validate using gt") {
    val gtZero1 = Validator.gt(0, (i: Int) => Option(s"$i was not greater than 0"))

    forAll { i: Int =>
      if (i > 0)
        gtZero1.run[Validated](i) should ===(Validated.valid(i))
      else
        gtZero1.run[Validated](i) should ===(Validated.invalid(NonEmptyList.of(s"$i was not greater than 0")))
    }

    val gtZero2 = Validator.gt(0, Option("Was not greater than 0"))

    forAll { i: Int =>
      if (i > 0)
        gtZero2.run[Validated](i) should ===(Validated.valid(i))
      else
        gtZero2.run[Validated](i) should ===(Validated.invalid(NonEmptyList.of("Was not greater than 0")))
    }
  }

  test("Validate using gteq") {
    val gteZero1 = Validator.gteq(0, (i: Int) => Option(s"$i was not greater than or equal to 0"))

    forAll { i: Int =>
      if (i >= 0)
        gteZero1.run[Validated](i) should ===(Validated.valid(i))
      else
        gteZero1.run[Validated](i) should ===(Validated.invalid(NonEmptyList.of(s"$i was not greater than or equal to 0")))
    }

    val gteZero2 = Validator.gteq(0, Option("Was not greater than or equal to 0"))

    forAll { i: Int =>
      if (i >= 0)
        gteZero2.run[Validated](i) should ===(Validated.valid(i))
      else
        gteZero2.run[Validated](i) should ===(Validated.invalid(NonEmptyList.of("Was not greater than or equal to 0")))
    }
  }

  test("Validate using lt") {
    val ltZero1 = Validator.lt(0, (i: Int) => Option(s"$i was not less than 0"))

    forAll { i: Int =>
      if (i < 0)
        ltZero1.run[Validated](i) should ===(Validated.valid(i))
      else
        ltZero1.run[Validated](i) should ===(Validated.invalid(NonEmptyList.of(s"$i was not less than 0")))
    }

    val ltZero2 = Validator.lt(0, Option("Was not less than 0"))

    forAll { i: Int =>
      if (i < 0)
        ltZero2.run[Validated](i) should ===(Validated.valid(i))
      else
        ltZero2.run[Validated](i) should ===(Validated.invalid(NonEmptyList.of("Was not less than 0")))
    }
  }

  test("Validate using lteq") {
    val lteZero1 = Validator.lteq(0, (i: Int) => Option(s"$i was not less than or equal to 0"))

    forAll { i: Int =>
      if (i <= 0)
        lteZero1.run[Validated](i) should ===(Validated.valid(i))
      else
        lteZero1.run[Validated](i) should ===(Validated.invalid(NonEmptyList.of(s"$i was not less than or equal to 0")))
    }

    val lteZero2 = Validator.lteq(0, Option("Was not less than or equal to 0"))

    forAll { i: Int =>
      if (i <= 0)
        lteZero2.run[Validated](i) should ===(Validated.valid(i))
      else
        lteZero2.run[Validated](i) should ===(Validated.invalid(NonEmptyList.of("Was not less than or equal to 0")))
    }
  }

  test("Validate using optional") {
    val lteZero = Validator.lteq(0, (i: Int) => Option(s"$i was not less than or equal to 0")).optional

    forAll { opt: Option[Int] =>
      opt match {
        case Some(i) if i <= 0 =>
          lteZero.run[Validated](opt) should ===(Validated.valid(opt))
        case Some(i) =>
          lteZero.run[Validated](opt) should ===(Validated.invalid(NonEmptyList.of(s"$i was not less than or equal to 0")))
        case None =>
          lteZero.run[Validated](opt) should ===(Validated.valid(opt))
      }
    }
  }

  test("Validate using required") {
    val lteZero = Validator.lteq(0, Option("Was not less than or equal to 0")).required(Option("I need an answer!"))

    forAll { opt: Option[Int] =>
      opt match {
        case Some(i) if i <= 0 =>
          lteZero.run[Validated](opt) should ===(Validated.valid(i))
        case Some(_) =>
          lteZero.run[Validated](opt) should ===(Validated.invalid(NonEmptyList.of("Was not less than or equal to 0")))
        case None =>
          lteZero.run[Validated](opt) should ===(Validated.invalid(NonEmptyList.of("I need an answer!")))
      }
    }
  }

  case class Document(maxColumn: Int, maxLines: Int, lines: List[String])

  object Document {
    implicit val eqDocument: Eq[Document] = Eq.fromUniversalEquals
  }

  test("Validate using has") {
    val error = "Maximum column width must be greater than zero"

    val documentValidator =
      Validator.validate[Option, String, Document]
        .has(_.maxColumn)(Validator.gt(0, Option(error)))

    val valid = Document(80, 120, Nil)
    val invalid = Document(0, 120, Nil)

    documentValidator.run[Validated](valid) should ===(Validated.Valid(valid))
    documentValidator.run[Validated](invalid) should ===(Validated.Invalid(NonEmptyList.of(error)))
  }

  test("Validate using has2") {
    val maxLinesValidator = Validator[Option, String, (Int, List[String])] {
      case (maxLines, lines) =>
        if (lines.length <= maxLines)
          None
        else
          Some(s"The number of lines in this document exceeds the maximum of $maxLines")
    }

    val documentValidator =
      Validator.validate[Option, String, Document]
        .has2(_.maxLines, _.lines)(maxLinesValidator)

    val valid = Document(80, 120, List("Hello", "World"))
    val invalid = Document(80, 1, List("Hello", "World"))

    documentValidator.run[Validated](valid) should ===(Validated.Valid(valid))

    documentValidator.run[Validated](invalid) should ===(Validated.Invalid(
      NonEmptyList.of("The number of lines in this document exceeds the maximum of 1")
    ))
  }

  test("Validate using all") {
    val emptyLineValidator =
      Validator[Option, String, String] { line =>
        if (line.length > 0)
          None
        else
          Some("Empty lines are not permitted")
      }

    val documentValidator =
      Validator.validate[Option, String, Document]
        .all(_.lines)(emptyLineValidator)

    val valid = Document(80, 120, List("Hello", "World"))
    val invalid = Document(80, 1, List("Hello", "", "World"))

    documentValidator.run[Validated](valid) should ===(Validated.Valid(valid))

    documentValidator.run[Validated](invalid) should ===(Validated.Invalid(
      NonEmptyList.of("Empty lines are not permitted")
    ))
  }

  test("Validate using all2") {
    val lineWidthValidator =
      Validator[Option, String, (Int, String)] {
        case (maxColumn, line) =>
          if (line.length <= maxColumn)
            None
          else
            Some(s"The line exceeds the maximum width of $maxColumn columns")
      }

    val documentValidator =
      Validator.validate[Option, String, Document]
        .all2(_.maxColumn, _.lines)(lineWidthValidator)

    val valid = Document(80, 120, List("Hello", "World"))
    val invalid = Document(4, 1, List("Hello", "", "World"))

    documentValidator.run[Validated](valid) should ===(Validated.Valid(valid))

    documentValidator.run[Validated](invalid) should ===(Validated.Invalid(
      NonEmptyList.of("The line exceeds the maximum width of 4 columns")
    ))
  }

  test("Validate using at") {
    val hasSecondLineValidator =
      Validator[Option, String, Option[String]] { maybeLine =>
        if (maybeLine.nonEmpty)
          None
        else
          Some("A document should have at least two lines")
      }

    val documentValidator =
      Validator.validate[Option, String, Document]
        .at(_.lines, 1)(hasSecondLineValidator)

    val valid = Document(80, 120, List("Hello", "World"))
    val invalid = Document(80, 120, List("Hello"))

    documentValidator.run[Validated](valid) should ===(Validated.Valid(valid))

    documentValidator.run[Validated](invalid) should ===(Validated.Invalid(
      NonEmptyList.of("A document should have at least two lines")
    ))
  }

  test("Validate using first") {
    val saysHelloValidator =
      Validator[Option, String, Option[String]](_.flatMap { line =>
        if (line.startsWith("Hello"))
          None
        else
          Some("A document should start with 'Hello'")
      })

    val documentValidator =
      Validator.validate[Option, String, Document]
        .first(_.lines)(saysHelloValidator)

    val valid = Document(80, 120, List("Hello", "World"))
    val invalid = Document(80, 120, List("Hi", "World"))

    documentValidator.run[Validated](valid) should ===(Validated.Valid(valid))

    documentValidator.run[Validated](invalid) should ===(Validated.Invalid(
      NonEmptyList.of("A document should start with 'Hello'")
    ))
  }
}
