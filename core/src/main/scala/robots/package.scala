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

import cats.{ Eq, MonoidK, Order, Traverse }

package object robots {
  type Validator[F[_], E, A] = PValidator[F, E, A, A]

  object Validator {
    def apply[F[_], E, A](validate: A => F[E])(implicit FF: Traverse[F], M: MonoidK[F]): Validator[F, E, A] =
      PValidator[F, E, A, A](validate)

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
}
