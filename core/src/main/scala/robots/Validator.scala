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

import cats.{ Applicative, ApplicativeError, ContravariantCartesian, Eq, Foldable, Monoid, MonoidK, Order, Traverse }
import cats.arrow.Choice
import cats.data.NonEmptyList
import cats.functor.Profunctor

final case class Validator[F[_], E, A](val validate: A => F[E])(implicit FF: Traverse[F], M: MonoidK[F]) {

  def run[G[_]](a: A)(implicit A: ApplicativeError[G, NonEmptyList[E]]): G[A] = {
    val fe = validate(a)

    if (FF.isEmpty(fe))
      A.pure(a)
    else
      A.raiseError(NonEmptyList.fromListUnsafe(FF.toList(fe)))
  }

  def over[M[_]](implicit FM: Foldable[M]): Validator[F, E, M[A]] =
    Validator(ma => FM.foldMap(ma)(validate)(M.algebra[E]))

  def optional: Validator[F, E, Option[A]] =
    Validator(_.map(validate).getOrElse(M.empty))

  def required(e: F[E]): Validator[F, E, Option[A]] =
    Validator(_.map(validate).getOrElse(e))

  def leftMap[EE](f: E => EE): Validator[F, EE, A] =
    Validator(a => FF.map(validate(a))(f))

  def dimap[EE, B](f: B => A)(g: E => EE): Validator[F, EE, B] =
    Validator(a => FF.map(validate(f(a)))(g))

  def contramap[B](f: B => A): Validator[F, E, B] =
    Validator(a => validate(f(a)))

  def and(that: Validator[F, E, A]): Validator[F, E, A] =
    Validator(a => M.combineK(this.validate(a), that.validate(a)))

  def or[B](that: Validator[F, E, B]): Validator[F, E, Either[A, B]] =
    Validator(_.fold(this.validate, that.validate))

  def has[B](f: A => B)(that: Validator[F, E, B]): Validator[F, E, A] =
    this and that.contramap(f)

  def has2[B, C](f: A => B, g: A => C)(that: Validator[F, E, (B, C)]): Validator[F, E, A] =
    this and that.contramap { a => (f(a), g(a)) }

  def all[M[_]: Foldable, B](f: A => M[B])(that: Validator[F, E, B]): Validator[F, E, A] =
    this and that.over[M].contramap(f)

  def all2[M[_], B, C](f: A => B, g: A => M[C])(that: Validator[F, E, (B, C)])(implicit TM: Traverse[M]): Validator[F, E, A] =
    this and that.over[M].contramap { a =>
      TM.map(g(a))(c => (f(a), c))
    }

  def at[M[_], B](f: A => M[B], i: Int)(that: Validator[F, E, Option[B]])(implicit FM: Foldable[M]): Validator[F, E, A] =
    this and that.contramap(a => FM.toList(f(a)).lift(i))

  def first[M[_], B](f: A => M[B])(that: Validator[F, E, Option[B]])(implicit FM: Foldable[M]): Validator[F, E, A] =
    this and that.contramap(a => FM.toList(f(a)).headOption)
}

object Validator extends ValidatorInstances with ValidatorFunctions

private[robots] sealed trait ValidatorFunctions {
  def validate[F[_]: Traverse, E, A](implicit M: MonoidK[F]): Validator[F, E, A] =
    Validator(_ => M.empty)

  def fail[F[_]: Traverse, E, A](e: F[E])(implicit M: MonoidK[F]): Validator[F, E, A] =
    Validator(_ => e)

  private def cmp[F[_]: Traverse, E, A](ref: A, msg: A => F[E])(valid: (A, A) => Boolean)(implicit M: MonoidK[F]): Validator[F, E, A] =
    Validator(in => if (valid(in, ref)) M.empty else msg(in))

  def eql[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit E: Eq[A]): Validator[F, E, A] =
    cmp(a, f)(E.eqv)

  def eql[F[_]: Traverse: MonoidK, E, A: Eq](a: A, e: F[E]): Validator[F, E, A] =
    eql(a, (_: A) => e)

  def neq[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit E: Eq[A]): Validator[F, E, A] =
    cmp(a, f)(E.neqv)

  def neq[F[_]: Traverse: MonoidK, E, A: Eq](a: A, e: F[E]): Validator[F, E, A] =
    neq(a, (_: A) => e)

  def gt[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit O: Order[A]): Validator[F, E, A] =
    cmp(a, f)(O.gt)

  def gt[F[_]: Traverse: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
    gt(a, (_: A) => e)

  def gteq[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit O: Order[A]): Validator[F, E, A] =
    cmp(a, f)(O.gteqv)

  def gteq[F[_]: Traverse: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
    gteq(a, (_: A) => e)

  def lt[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit O: Order[A]): Validator[F, E, A] =
    cmp(a, f)(O.lt)

  def lt[F[_]: Traverse: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
    lt(a, (_: A) => e)

  def lteq[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit O: Order[A]): Validator[F, E, A] =
    cmp(a, f)(O.lteqv)

  def lteq[F[_]: Traverse: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
    lteq(a, (_: A) => e)

}

private[robots] sealed abstract class ValidatorInstances {

  implicit def robotsMonoidForValidator[F[_], E, A](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]
  ): Monoid[Validator[F, E, A]] =
    new ValidatorMonoid[F, E, A] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsMonoidKForValidator[F[_], E, A](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]
  ): MonoidK[Validator[F, E, ?]] =
    new ValidatorMonoidK[F, E] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsContravariantCartesianForValidator[F[_], E](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]
  ): ContravariantCartesian[Validator[F, E, ?]] =
    new ValidatorContravariantCartesian[F, E] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsProfunctorForValidator[F[_]](
    implicit
    F0: Traverse[F],
    M0: MonoidK[F]
  ): Profunctor[Lambda[(A, E) => Validator[F, E, A]]] =
    new ValidatorProfunctor[F] {
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsApplicativeForValidator[F[_], R](
    implicit
    A0: Applicative[F],
    F0: Traverse[F],
    M0: MonoidK[F]
  ): Applicative[Validator[F, ?, R]] =
    new ValidatorApplicative[F, R] {
      def A: Applicative[F] = A0
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }

  implicit def robotsChoiceForValidator[F[_]](
    implicit
    A0: Applicative[F],
    F0: Traverse[F],
    M0: MonoidK[F]
  ): Choice[Lambda[(A, E) => Validator[F, E, A]]] =
    new ValidatorChoice[F] {
      def A: Applicative[F] = A0
      def F: Traverse[F] = F0
      def M: MonoidK[F] = M0
    }
}

private trait ValidatorMonoid[F[_], E, A]
    extends Monoid[Validator[F, E, A]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  def empty: Validator[F, E, A] =
    Validator.validate[F, E, A]

  def combine(x: Validator[F, E, A], y: Validator[F, E, A]): Validator[F, E, A] =
    x and y
}

private trait ValidatorMonoidK[F[_], E]
    extends MonoidK[Validator[F, E, ?]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  def empty[A]: Validator[F, E, A] =
    Validator.validate[F, E, A]

  def combineK[A](x: Validator[F, E, A], y: Validator[F, E, A]): Validator[F, E, A] =
    x and y
}

private trait ValidatorApplicative[F[_], R]
    extends Applicative[Validator[F, ?, R]] {
  implicit def A: Applicative[F]
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  def pure[A](a: A): Validator[F, A, R] =
    Validator.fail(A.pure(a))

  def ap[A, B](ff: Validator[F, A => B, R])(fa: Validator[F, A, R]): Validator[F, B, R] =
    Validator(r => A.ap(ff.validate(r))(fa.validate(r)))
}

private trait ValidatorContravariantCartesian[F[_], E]
    extends ContravariantCartesian[Validator[F, E, ?]] {
  implicit def F: Traverse[F]
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
  implicit def F: Traverse[F]
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

private trait ValidatorProfunctor[F[_]]
    extends Profunctor[Lambda[(A, E) => Validator[F, E, A]]] {
  implicit def F: Traverse[F]
  implicit def M: MonoidK[F]

  override def lmap[A, B, C](fab: Validator[F, B, A])(f: C => A): Validator[F, B, C] =
    fab.contramap(f)

  override def rmap[A, B, C](fab: Validator[F, B, A])(f: B => C): Validator[F, C, A] =
    fab.leftMap(f)

  def dimap[A, B, C, D](fab: Validator[F, B, A])(f: C => A)(g: B => D): Validator[F, D, C] =
    fab.dimap(f)(g)
}
