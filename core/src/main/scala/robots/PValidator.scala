/*
 * Copyright 2017 David Gregory and the Robots project contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package robots

import cats.{ ~>, Applicative, ApplicativeError, Foldable, MonoidK, Semigroup, SemigroupK, Traverse }
import cats.arrow.{ Category, Choice, Profunctor }
import cats.Contravariant
import cats.data.NonEmptyList

final case class PValidator[F[_], E, A, B](val validate: A => F[E], f: A => B)(implicit FF: Traverse[F], M: MonoidK[F]) {
  def runWith[G[_], EE](a: A)(g: F[E] => EE)(implicit A: ApplicativeError[G, EE]): G[B] = {
    val fe = validate(a)

    if (FF.isEmpty(fe))
      A.pure(f(a))
    else
      A.raiseError(g(fe))
  }

  def run[G[_]](a: A)(implicit A: ApplicativeError[G, F[E]]): G[B] =
    runWith(a)(identity)

  def run_[G[_]](a: A)(implicit A: ApplicativeError[G, Unit]): G[B] =
    runWith(a)(_ => ())

  def runK[G[_], H[_]](a: A)(g: F ~> H)(implicit A: ApplicativeError[G, H[E]]): G[B] =
    runWith(a)(g.apply)

  def runNel[G[_, _]](a: A)(implicit A: ApplicativeError[G[NonEmptyList[E], ?], NonEmptyList[E]]): G[NonEmptyList[E], B] =
    runWith(a)(fe => NonEmptyList.fromListUnsafe(FF.toList(fe)))

  def andThen[C](that: PValidator[F, E, B, C]): PValidator[F, E, A, C] =
    PValidator[F, E, A, C]({ a =>
      M.combineK(this.validate(a), that.validate(f(a)))
    }, this.f andThen that.f)

  def map[C](g: B => C): PValidator[F, E, A, C] = copy(f = g compose f)

  def map2[C, D](that: PValidator[F, E, A, C])(g: (B, C) => D): PValidator[F, E, A, D] =
    this.product(that).map(Function.tupled(g))

  def over[M[_]](implicit TM: Traverse[M]): PValidator[F, E, M[A], M[B]] =
    Validator[F, E, M[A]] { ma =>
      TM.foldMap(ma)(validate)(M.algebra[E])
    }.map(ma => TM.map(ma)(f))

  def optional: PValidator[F, E, Option[A], Option[B]] =
    Validator[F, E, Option[A]] { opt =>
      opt.map(validate).getOrElse(M.empty)
    }.map(_.map(f))

  def required(e: F[E]): PValidator[F, E, Option[A], B] =
    Validator[F, E, Option[A]] { opt =>
      opt.map(validate).getOrElse(e)
    }.map(opt => f(opt.get))

  def leftMap[EE](g: E => EE): PValidator[F, EE, A, B] =
    PValidator(a => FF.map(validate(a))(g), f)

  def dimap[Z, C](g: Z => A)(h: B => C): PValidator[F, E, Z, C] =
    PValidator(z => validate(g(z)), g andThen f andThen h)

  def contramap[Z](g: Z => A): PValidator[F, E, Z, B] =
    PValidator(a => validate(g(a)), g andThen f)

  def and(that: PValidator[F, E, A, _]): PValidator[F, E, A, B] =
    PValidator(a => M.combineK(this.validate(a), that.validate(a)), f)

  def product[C](that: PValidator[F, E, A, C]): PValidator[F, E, A, (B, C)] =
    PValidator(a => M.combineK(this.validate(a), that.validate(a)), { a =>
      (this.f(a), that.f(a))
    })

  def or[C](that: PValidator[F, E, C, B]): PValidator[F, E, Either[A, C], B] =
    PValidator(_.fold(this.validate, that.validate), _.fold(this.f, that.f))

  def has[C](f: A => C)(that: PValidator[F, E, C, _]): PValidator[F, E, A, B] =
    this and that.contramap(f)

  def has2[C, D](f: A => C, g: A => D)(that: PValidator[F, E, (C, D), _]): PValidator[F, E, A, B] =
    this and that.contramap { a => (f(a), g(a)) }

  def all[M[_]: Traverse, C](f: A => M[C])(that: PValidator[F, E, C, _]): PValidator[F, E, A, B] =
    this and that.over[M].contramap(f)

  def all2[M[_], C, D](f: A => C, g: A => M[D])(that: PValidator[F, E, (C, D), _])(implicit TM: Traverse[M]): PValidator[F, E, A, B] =
    this and that.over[M].contramap { a =>
      TM.map(g(a))(c => (f(a), c))
    }

  def allIndexed[M[_], C](g: A => M[C])(that: PValidator[F, E, (C, Int), _])(implicit TM: Traverse[M]): PValidator[F, E, A, B] =
    this and that.over[M].contramap { a =>
      TM.zipWithIndex(g(a))
    }

  def at[M[_], C](f: A => M[C], i: Long)(that: PValidator[F, E, Option[C], _])(implicit TM: Traverse[M]): PValidator[F, E, A, B] =
    this and that.contramap(a => TM.get(f(a))(i))

  def first[M[_], C](f: A => M[C])(that: PValidator[F, E, Option[C], _])(implicit FM: Foldable[M]): PValidator[F, E, A, B] =
    this and that.contramap(a => FM.get(f(a))(0))
}

object PValidator extends PValidatorInstances

private[robots] sealed abstract class PValidatorInstances {

  implicit def robotsApplicativeForPValidator[F[_], E, A](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]): Applicative[PValidator[F, E, A, ?]] =
    new PValidatorApplicative[F, E, A] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsChoiceForPValidator[F[_], E](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]): Choice[PValidator[F, E, ?, ?]] =
    new PValidatorChoice[F, E] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsContravariantForPValidator[F[_], E, B](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]): Contravariant[PValidator[F, E, ?, B]] =
    new PValidatorContravariant[F, E, B] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsProfunctorForPValidator[F[_], E](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]): Profunctor[PValidator[F, E, ?, ?]] =
    new PValidatorProfunctor[F, E] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsSemigroupForPValidator[F[_], E, A, B](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]): Semigroup[PValidator[F, E, A, B]] =
    new PValidatorSemigroup[F, E, A, B] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsSemigroupKForPValidator[F[_], E, B](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]): SemigroupK[PValidator[F, E, ?, B]] =
    new PValidatorSemigroupK[F, E, B] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }
}

private trait PValidatorApplicative[F[_], E, A]
  extends Applicative[PValidator[F, E, A, ?]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  def pure[B](b: B): PValidator[F, E, A, B] =
    PValidator(_ => M.empty, _ => b)

  def ap[B, C](ff: PValidator[F, E, A, B => C])(fa: PValidator[F, E, A, B]): PValidator[F, E, A, C] =
    ff.map2(fa) { case (f, a) => f(a) }
}

private trait PValidatorCategory[F[_], E]
  extends Category[PValidator[F, E, ?, ?]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  def id[A]: PValidator[F, E, A, A] =
    Validator(_ => M.empty)

  def compose[A, B, C](
    f: PValidator[F, E, B, C],
    g: PValidator[F, E, A, B]): PValidator[F, E, A, C] = g andThen f
}

private trait PValidatorChoice[F[_], E]
  extends PValidatorCategory[F, E]
  with Choice[PValidator[F, E, ?, ?]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  def choice[A, B, C](
    f: PValidator[F, E, A, C],
    g: PValidator[F, E, B, C]): PValidator[F, E, Either[A, B], C] = f or g
}

private trait PValidatorContravariant[F[_], E, B]
  extends Contravariant[PValidator[F, E, ?, B]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  def contramap[A, Z](validator: PValidator[F, E, A, B])(f: Z => A): PValidator[F, E, Z, B] =
    validator.contramap(f)
}

private trait PValidatorProfunctor[F[_], E]
  extends Profunctor[PValidator[F, E, ?, ?]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  override def lmap[A, B, C](fab: PValidator[F, E, A, B])(f: C => A): PValidator[F, E, C, B] =
    fab.contramap(f)

  override def rmap[A, B, C](fab: PValidator[F, E, A, B])(f: B => C): PValidator[F, E, A, C] =
    fab.map(f)

  def dimap[A, B, C, D](fab: PValidator[F, E, A, B])(f: C => A)(g: B => D): PValidator[F, E, C, D] =
    fab.dimap(f)(g)
}

private trait PValidatorSemigroup[F[_], E, A, B]
  extends Semigroup[PValidator[F, E, A, B]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  def combine(x: PValidator[F, E, A, B], y: PValidator[F, E, A, B]): PValidator[F, E, A, B] =
    x and y
}

private trait PValidatorSemigroupK[F[_], E, B]
  extends SemigroupK[PValidator[F, E, ?, B]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  def combineK[A](x: PValidator[F, E, A, B], y: PValidator[F, E, A, B]): PValidator[F, E, A, B] =
    x and y
}
