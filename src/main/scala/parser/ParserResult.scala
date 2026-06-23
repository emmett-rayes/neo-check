package neocheck
package parser

import scala.util.Try

/** The result of a parser, containing either the remaining input and the parsed output, or a parsing error.
 *
 * @tparam Input  the type of the input to be parsed.
 * @tparam Output the type of the output produced by the parser.
 */
type ParserResult[Input, +Output] = Try[(remaining: Input, parsed: Output)]
