package neocheck
package parser

import parser.ParserCombinators.*

/** A catalogue of reusable, interpreter-agnostic parser programs.
 *
 * Each program is a polymorphic function describing the structure of a parser purely in terms of a [[ParserAlgebra]],
 * without committing to any concrete representation of `Parser`.
 *
 * Results are left as the algebra's own output types, i.e. the path-dependent `P.Output[...]`,
 * so each interpreter decides what those types mean.
 */
object ParserPrograms {

  import ParserKind.*

  /** Matches the single literal word `"hello"`. */
  def literalHello[Parser[_]](using P: ParserAlgebra[Parser]): Parser[P.Output[Literal]] = {
    P.literal("hello")
  }

  /** Matches a non-empty run of digits via a regular expression. */
  def regexDigits[Parser[_]](using P: ParserAlgebra[Parser]): Parser[P.Output[Regex]] = {
    P.regex("[0-9]+".r)
  }

  /** Always succeeds with the constant `42`, consuming no input.
   *
   * Exercises the unit (one) element of the algebra.
   */
  def constantSuccess[Parser[_]](using P: ParserAlgebra[Parser]): Parser[Int] = {
    P.success(42)
  }

  /** Always fails, consuming no input.
   *
   * Exercises the zero element of the algebra.
   */
  def alwaysFailure[Parser[_]](using P: ParserAlgebra[Parser]): Parser[Nothing] = {
    P.failure[Nothing]("boom")
  }

  /** Matches `"hello"` then `"world"`, pairing both results.
   *
   * Exercises sequencing via [[ParserCombinators.andThen]] (and hence `flatMap`).
   */
  def helloThenWorld[Parser[_]](using P: ParserAlgebra[Parser])
  : Parser[(P.Output[Literal], P.Output[Literal])] = {
    P.literal("hello").andThen(P.literal("world"))
  }

  /** Matches either `"true"` or `"false"`.
   *
   * Exercises alternation via [[ParserAlgebra.orElse]].
   */
  def trueOrFalse[Parser[_]](using P: ParserAlgebra[Parser]): Parser[P.Output[Literal]] = {
    P.literal("true").orElse(P.literal("false"))
  }

  /** Matches `"ab"` greedily zero or more times, collecting each match.
   *
   * Exercises [[ParserCombinators.repeated]].
   */
  def repeatedAb[Parser[_]](using P: ParserAlgebra[Parser]): Parser[List[P.Output[Literal]]] = {
    P.literal("ab").repeated
  }

  /** Matches `"ab"` greedily, requiring at least two occurrences.
   *
   * Exercises [[ParserCombinators.atLeast]].
   */
  def atLeastTwoAb[Parser[_]](using P: ParserAlgebra[Parser]): Parser[List[P.Output[Literal]]] = {
    P.literal("ab").atLeast(2)
  }

  /** Matches a run of digits wrapped in parentheses, keeping only the digits.
   *
   * Exercises [[ParserCombinators.between]].
   */
  def digitsInParens[Parser[_]](using P: ParserAlgebra[Parser]): Parser[P.Output[Regex]] = {
    P.regex("[0-9]+".r).between(P.literal("("), P.literal(")"))
  }

  /** Matches a `">"` prefix followed by digits, keeping only the digits.
   *
   * Exercises [[ParserCombinators.skipThen]].
   */
  def prefixedDigits[Parser[_]](using P: ParserAlgebra[Parser]): Parser[P.Output[Regex]] = {
    P.literal(">").skipThen(P.regex("[0-9]+".r))
  }

  /** Matches digits followed by a `";"` terminator, keeping only the digits.
   *
   * Exercises [[ParserCombinators.thenSkip]].
   */
  def terminatedDigits[Parser[_]](using P: ParserAlgebra[Parser]): Parser[P.Output[Regex]] = {
    P.regex("[0-9]+".r).thenSkip(P.literal(";"))
  }

  /** A comma-separated, parenthesized list of digit groups, e.g. `(1,2,3)`.
   *
   * Combines several primitives and combinators (`regex`, `literal`, `andThen`,
   * `repeated`, `skipThen`, `between`, `map`) into a single non-trivial program,
   * yielding the list of digit groups in order.
   */
  def digitListInParens[Parser[_]](using P: ParserAlgebra[Parser]): Parser[List[P.Output[Regex]]] = {
    val digits = P.regex("[0-9]+".r)
    val tail = P.literal(",").skipThen(digits).repeated
    val list = digits.andThen(tail).map((head, rest) => head :: rest)
    list.between(P.literal("("), P.literal(")"))
  }
}
