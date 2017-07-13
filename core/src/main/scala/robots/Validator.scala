package robots

import cats.{ Applicative, ContravariantCartesian, Eq, Foldable, MonoidK, Order, Traverse }
import cats.arrow.Choice
import cats.data.{ NonEmptyList, Validated, ValidatedNel }
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.semigroupk._
import cats.syntax.order._

final case class Validator[F[_]: Foldable, E, A](private[robots] val validate: A => F[E])(implicit M: MonoidK[F]) {

  def run(a: A): ValidatedNel[E, A] = {
    val fe = validate(a)

    if (fe.isEmpty)
      Validated.Valid(a)
    else
      Validated.Invalid(NonEmptyList.fromListUnsafe(fe.toList))
  }

  def over[M[_]: Foldable]: Validator[F, E, M[A]] =
    Validator(_.foldMap(validate)(M.algebra[E]))

  def optional: Validator[F, E, Option[A]] =
    Validator(_.map(validate).getOrElse(M.empty))

  def required(e: F[E]): Validator[F, E, Option[A]] =
    Validator(_.map(validate).getOrElse(e))

  def contramap[B](f: B => A): Validator[F, E, B] =
    Validator(a => validate(f(a)))

  def and(that: Validator[F, E, A]): Validator[F, E, A] =
    Validator(a => this.validate(a) <+> that.validate(a))

  def or[B](that: Validator[F, E, B]): Validator[F, E, Either[A, B]] =
    Validator(_.fold(this.validate, that.validate))

  def has[B](f: A => B)(that: Validator[F, E, B]): Validator[F, E, A] =
    this and that.contramap(f)

  def has2[B, C](f: A => B, g: A => C)(that: Validator[F, E, (B, C)]): Validator[F, E, A] =
    this and that.contramap { a => (f(a), g(a)) }

  def all[M[_]: Foldable, B](f: A => M[B])(that: Validator[F, E, B]): Validator[F, E, A] =
    this and that.over[M].contramap(f)

  def all2[M[_]: Traverse, B, C](f: A => B, g: A => M[C])(that: Validator[F, E, (B, C)]): Validator[F, E, A] =
    this and that.over[M].contramap { a =>
      g(a).map(c => (f(a), c))
    }

  def at[M[_]: Foldable, B](f: A => M[B], i: Int)(that: Validator[F, E, Option[B]]): Validator[F, E, A] =
    this and that.contramap(a => f(a).toList.lift(i))

  def first[M[_]: Foldable, B](f: A => M[B])(that: Validator[F, E, Option[B]]): Validator[F, E, A] =
    this and that.contramap(a => f(a).toList.headOption)
}

object Validator extends ValidatorInstances with ValidatorFunctions

private[robots] sealed trait ValidatorFunctions {
  def validate[F[_]: Foldable, E, A](implicit M: MonoidK[F]): Validator[F, E, A] =
    Validator(_ => M.empty)

  private def cmp[F[_]: Foldable, E, A](ref: A, msg: A => F[E])(valid: (A, A) => Boolean)(implicit M: MonoidK[F]): Validator[F, E, A] =
    Validator(in => if (valid(in, ref)) M.empty else msg(in))

  def eql[F[_]: Foldable: MonoidK, E, A: Eq](a: A, f: A => F[E]): Validator[F, E, A] =
    cmp(a, f)(_ === _)

  def eql[F[_]: Foldable: MonoidK, E, A: Eq](a: A, e: F[E]): Validator[F, E, A] =
    eql(a, (_: A) => e)

  def neq[F[_]: Foldable: MonoidK, E, A: Eq](a: A, f: A => F[E]): Validator[F, E, A] =
    cmp(a, f)(_ =!= _)

  def neq[F[_]: Foldable: MonoidK, E, A: Eq](a: A, e: F[E]): Validator[F, E, A] =
    neq(a, (_: A) => e)

  def gt[F[_]: Foldable: MonoidK, E, A: Order](a: A, f: A => F[E]): Validator[F, E, A] =
    cmp(a, f)(_ > _)

  def gt[F[_]: Foldable: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
    gt(a, (_: A) => e)

  def gteq[F[_]: Foldable: MonoidK, E, A: Order](a: A, f: A => F[E]): Validator[F, E, A] =
    cmp(a, f)(_ >= _)

  def gteq[F[_]: Foldable: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
    gteq(a, (_: A) => e)

  def lt[F[_]: Foldable: MonoidK, E, A: Order](a: A, f: A => F[E]): Validator[F, E, A] =
    cmp(a, f)(_ < _)

  def lt[F[_]: Foldable: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
    lt(a, (_: A) => e)

  def lteq[F[_]: Foldable: MonoidK, E, A: Order](a: A, f: A => F[E]): Validator[F, E, A] =
    cmp(a, f)(_ <= _)

  def lteq[F[_]: Foldable: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
    lteq(a, (_: A) => e)

}

private[robots] sealed abstract class ValidatorInstances {

  implicit def robotsContravariantCartesianForValidator[F[_], E](
    implicit
    F0: Foldable[F],
    M0: MonoidK[F]
  ): ContravariantCartesian[Validator[F, E, ?]] =
    new ValidatorContravariantCartesian[F, E] {
      def F: Foldable[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsChoiceForValidator[F[_]](
    implicit
    A0: Applicative[F],
    F0: Foldable[F],
    M0: MonoidK[F]
  ): Choice[Lambda[(A, E) => Validator[F, E, A]]] =
    new ValidatorChoice[F] {
      def A: Applicative[F] = A0
      def F: Foldable[F] = F0
      def M: MonoidK[F] = M0
    }
}

private trait ValidatorContravariantCartesian[F[_], E]
    extends ContravariantCartesian[Validator[F, E, ?]] {
  implicit def F: Foldable[F]
  implicit def M: MonoidK[F]

  def contramap[A, B](validator: Validator[F, E, A])(f: B => A): Validator[F, E, B] =
    validator.contramap(f)

  def product[A, B](va: Validator[F, E, A], vb: Validator[F, E, B]): Validator[F, E, (A, B)] =
    Validator.validate[F, E, (A, B)]
      .has(_._1)(va)
      .has(_._2)(vb)
}

private trait ValidatorChoice[F[_]]
    extends Choice[Lambda[(A, E) => Validator[F, E, A]]] {
  implicit def A: Applicative[F]
  implicit def F: Foldable[F]
  implicit def M: MonoidK[F]

  def id[A]: Validator[F, A, A] = Validator(A.pure)

  def compose[A, B, C](
    f: Validator[F, C, B],
    g: Validator[F, B, A]
  ): Validator[F, C, A] =
    Validator(a => f.over[F].validate(g.validate(a)))

  def choice[A, B, C](
    f: Validator[F, C, A],
    g: Validator[F, C, B]
  ): Validator[F, C, Either[A, B]] = f or g
}
