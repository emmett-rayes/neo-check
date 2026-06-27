package neocheck
package parser

/** A parser that takes an input of type `Input` and produces an output of type `Output`.
 *
 * @tparam Input  the type of the input to be parsed.
 * @tparam Output the type of the output produced by the parser.
 */
trait Parser[Input, +Output] {

  /** Parses the given input, returning the parsed output and the remaining input or a parsing error.
   *
   * @param input the input to parse.
   * @return the parse result, containing either the parsed output and the remaining input, or a parsing error.
   */
  def parse(input: Input): ParserResult[Input, Output]
}
