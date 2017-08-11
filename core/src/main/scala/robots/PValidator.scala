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

import cats.{ ApplicativeError, Foldable, MonoidK, Traverse }
import cats.data.NonEmptyList

final case class PValidator[F[_], E, A, B](val validate: A => F[E], f: A => B = identity[A] _)(implicit FF: Traverse[F], M: MonoidK[F]) {

  def run[G[_, _]](a: A)(implicit A: ApplicativeError[G[NonEmptyList[E], ?], NonEmptyList[E]]): G[NonEmptyList[E], B] = {
    val fe = validate(a)

    if (FF.isEmpty(fe))
      A.pure(f(a))
    else
      A.raiseError(NonEmptyList.fromListUnsafe(FF.toList(fe)))
  }

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

  def at[M[_], C](f: A => M[C], i: Int)(that: PValidator[F, E, Option[C], _])(implicit TM: Traverse[M]): PValidator[F, E, A, B] =
    this and that.contramap(a => TM.toList(f(a)).lift(i))

  def first[M[_], C](f: A => M[C])(that: PValidator[F, E, Option[C], _])(implicit FM: Foldable[M]): PValidator[F, E, A, B] =
    this and that.contramap(a => FM.toList(f(a)).headOption)
}
