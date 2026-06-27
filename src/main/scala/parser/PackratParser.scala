package neocheck
package parser

import scala.collection.mutable

/** A type constructor for memoizing parsers that produce an output of type `Output`.
 *
 * @tparam Input the type of the input to be parsed.
 */
type PackratParserF[Input] = [Output] =>> PackratParser[Input, Output]

/** A parser that wraps `underlying` and memoizes its results per input.
 *
 * @param underlying the parser to be memoized.
 * @tparam Input  the type of the input to be parsed.
 * @tparam Output the type of the output produced by the parser.
 */
final class PackratParser[Input, Output](underlying: Parser[Input, Output]) extends Parser[Input, Output] {
  private val memo = mutable.Map.empty[Input, ParserResult[Input, Output]]

  override def parse(input: Input): ParserResult[Input, Output] = {
    memo.getOrElseUpdate(input, underlying.parse(input))
  }
}
