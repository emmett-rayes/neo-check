package neocheck
package parser

import scala.util.matching.Regex


/** A parser typeclass algebra that defines the basic operations of a parser.
 *
 * @tparam Parser the higher-kinded type of the parser.
 */
trait ParserAlgebra[Parser[_]] {
  /** A type-level mapping from a [[ParserKind]] to the corresponding output
   * type produced by a parser of that base kind.
   *
   * @tparam K the kind of parser whose output type is being resolved.
   */
  type Output[K <: ParserKind]

  /** Creates a parser that matches a literal string.
   *
   * @param expected the literal string to match.
   * @return a parser that produces the matched literal string.
   */
  def literal(expected: String): Parser[Output[ParserKind.Literal]]

  /** Creates a parser that matches a regular expression.
   *
   * @param expected the regular expression to match.
   * @return a parser that produces the matched regular expression.
   */
  def regex(expected: Regex): Parser[Output[ParserKind.Regex]]

  /** Creates a parser that always succeeds with the given output, consuming no input.
   *
   * This functions as the unit (one) element of the parser algebra.
   *
   * @param output the output to produce.
   * @tparam Output the type of the output.
   * @return a parser that always succeeds with `output`.
   */
  def success[Output](output: Output): Parser[Output]

  /** Creates a parser that always fails with the given message, consuming no input.
   *
   * This functions as the zero element of the parser algebra.
   *
   * @param message the error message to produce.
   * @tparam Output the type of the output.
   * @return a parser that always fails with `message`.
   */
  def failure[Output](message: String): Parser[Output]

  /** Creates a recursive parser by passing itself to the given function `p`.
   *
   * This functions as the least fixed point operation of the parser algebra.
   *
   * @param p a function that takes a parser and returns a new parser, potentially using the input parser recursively.
   * @return a parser that can be defined recursively using `p`.
   */
  def recursive[Output](p: Parser[Output] => Parser[Output]): Parser[Output]

  /** Creates a named tuple of recursive parsers by passing themselves to the given function `p`.
   *
   * This functions as the least fixed point operation of the parser algebra lifted to named tuples.
   * The names of `Outputs` are preserved on the resulting parsers, allowing mutually recursive
   * parsers to be referred to by label rather than by position.
   *
   * @param p a function that takes a named tuple of parsers and returns a new named tuple of parsers,
   *          potentially using the input parsers recursively.
   *
   * @return a named tuple of parsers that can be defined recursively using `p`.
   */
  def recursive[Outputs <: NamedTuple.AnyNamedTuple]
  (p: NamedTuple.Map[Outputs, Parser] => NamedTuple.Map[Outputs, Parser])
  (using size: ValueOf[NamedTuple.Size[Outputs]]): NamedTuple.Map[Outputs, Parser]

  /** Parser algebra operations available on any [[Parser]].
   *
   * @param self the parser to extend.
   * @tparam Output the result type produced by `self`.
   */
  extension [Output](self: Parser[Output]) {
    /** Sequences `self` with a parser derived from its result.
     *
     * Runs `self` and, if it succeeds, feeds its `Output` to `f` to get the
     * next parser, which is then run on the remaining input.
     *
     * This functions as the multiplicative (sequencing) operation of the parser algebra.
     *
     * @param f maps the `Output` of `self` to the next parser to run.
     * @tparam Mapped the result type of the parser produced by `f`.
     * @return a parser running `self`, then the parser produced by `f`.
     */
    def flatMap[Mapped](f: Output => Parser[Mapped]): Parser[Mapped]

    /** Tries `self`, falling back to `other` if it fails.
     *
     * Runs `self` and, only if it fails, runs `other` instead.
     *
     * This functions as the additive (alternation) operation of the parser algebra.
     *
     * @note `other` is evaluated lazily, so it is not evaluated if `self` succeeds.
     *
     * @param other the parser to try if `self` fails.
     * @tparam Else the result type of `other`.
     * @return a parser producing the result of `self`, or of `other` on failure.
     */
    def orElse[Else](other: Parser[Else]): Parser[Output | Else]
  }
}
