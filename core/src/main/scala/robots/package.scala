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
  /**
   * A `Validator[F[_], E, A]` wraps a validation function `A => F[E]` and provides functions to accumulate the results of this validation into different structures
   * and to combine validations together so that validations for properties of an object can be combined together to form a validator for the entire object.
   */
  type Validator[F[_], E, A] = PValidator[F, E, A, A]

  /**
   * The companion object for `Validator` provides a number of functions for constructing simple validators.
   *
   * Validators for comparisons and equality checking can be constructed for types with `Order` and `Eq` instances, respectively.
   *
   * There are also functions provided for constructing validators which always pass and which always fail.
   */
  object Validator {
    /**
     * Construct a validator by wrapping up a function `A => F[E]`.
     */
    def apply[F[_]: Traverse, E, A](validate: A => F[E])(implicit M: MonoidK[F]): Validator[F, E, A] =
      PValidator[F, E, A, A](validate, identity)

    /**
     * Construct a validator for `A` which always passes. Usually used as the basis for a more complex validator.
     */
    def validate[F[_]: Traverse, E, A](implicit M: MonoidK[F]): Validator[F, E, A] =
      Validator(_ => M.empty)

    /**
     * Construct a validator for `A` which always fails with the given error messages.
     */
    def fail[F[_]: Traverse, E, A](e: F[E])(implicit M: MonoidK[F]): Validator[F, E, A] =
      Validator(_ => e)

    /**
     * Construct a validator for `A` which validates the input against the reference value `ref` using a validation function `valid`.
     *
     * If the result is a failure the function `msg` is used to construct error messages based upon the input.
     */
    private def cmp[F[_]: Traverse, E, A](ref: A, msg: A => F[E])(valid: (A, A) => Boolean)(implicit M: MonoidK[F]): Validator[F, E, A] =
      Validator(in => if (valid(in, ref)) M.empty else msg(in))

    /**
     * Construct a validator for `A` which validates that the input is equal to the reference value `a` according to the `Eq` instance for `A`.
     *
     * If the validation fails the function `f` is used to construct validation error messages based upon the input.
     */
    def eql[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit E: Eq[A]): Validator[F, E, A] =
      cmp(a, f)(E.eqv)

    /**
     * Construct a validator for `A` which validates that the input is equal to the reference value `a` according to the `Eq` instance for `A`.
     *
     * If the validation fails the error messages `e` are returned.
     */
    def eql[F[_]: Traverse: MonoidK, E, A: Eq](a: A, e: F[E]): Validator[F, E, A] =
      eql(a, (_: A) => e)

    /**
     * Construct a validator for `A` which validates that the input is not equal to the reference value `a` according to the `Eq` instance for `A`.
     *
     * If the validation fails the function `f` is used to construct validation error messages based upon the input.
     */
    def neq[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit E: Eq[A]): Validator[F, E, A] =
      cmp(a, f)(E.neqv)

    /**
     * Construct a validator for `A` which validates that the input is not equal to the reference value `a` according to the `Eq` instance for `A`.
     *
     * If the validation fails the error messages `e` are returned.
     */
    def neq[F[_]: Traverse: MonoidK, E, A: Eq](a: A, e: F[E]): Validator[F, E, A] =
      neq(a, (_: A) => e)

    /**
     * Construct a validator for `A` which validates that the input is greater than the reference value `a` according to the `Order` instance for `A`.
     *
     * If the validation fails the function `f` is used to construct validation error messages based upon the input.
     */
    def gt[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit O: Order[A]): Validator[F, E, A] =
      cmp(a, f)(O.gt)

    /**
     * Construct a validator for `A` which validates that the input is greater than the reference value `a` according to the `Order` instance for `A`.
     *
     * If the validation fails the error messages `e` are returned.
     */
    def gt[F[_]: Traverse: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
      gt(a, (_: A) => e)

    /**
     * Construct a validator for `A` which validates that the input is greater than or equal to the reference value `a` according to the `Order` instance for `A`.
     *
     * If the validation fails the function `f` is used to construct validation error messages based upon the input.
     */
    def gteq[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit O: Order[A]): Validator[F, E, A] =
      cmp(a, f)(O.gteqv)

    /**
     * Construct a validator for `A` which validates that the input is greater than or equal to the reference value `a` according to the `Order` instance for `A`.
     *
     * If the validation fails the error messages `e` are returned.
     */
    def gteq[F[_]: Traverse: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
      gteq(a, (_: A) => e)

    /**
     * Construct a validator for `A` which validates that the input is less than the reference value `a` according to the `Order` instance for `A`.
     *
     * If the validation fails the function `f` is used to construct validation error messages based upon the input.
     */
    def lt[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit O: Order[A]): Validator[F, E, A] =
      cmp(a, f)(O.lt)

    /**
     * Construct a validator for `A` which validates that the input is less than the reference value `a` according to the `Order` instance for `A`.
     *
     * If the validation fails the error messages `e` are returned.
     */
    def lt[F[_]: Traverse: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
      lt(a, (_: A) => e)

    /**
     * Construct a validator for `A` which validates that the input is less than or equal to the reference value `a` according to the `Order` instance for `A`.
     *
     * If the validation fails the function `f` is used to construct validation error messages based upon the input.
     */
    def lteq[F[_]: Traverse: MonoidK, E, A](a: A, f: A => F[E])(implicit O: Order[A]): Validator[F, E, A] =
      cmp(a, f)(O.lteqv)

    /**
     * Construct a validator for `A` which validates that the input is less than or equal to the reference value `a` according to the `Order` instance for `A`.
     *
     * If the validation fails the error messages `e` are returned.
     */
    def lteq[F[_]: Traverse: MonoidK, E, A: Order](a: A, e: F[E]): Validator[F, E, A] =
      lteq(a, (_: A) => e)
  }
}
