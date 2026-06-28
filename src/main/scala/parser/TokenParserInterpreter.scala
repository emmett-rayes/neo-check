package neocheck
package parser

import scala.util.Try
import scala.util.matching.Regex

/** A parser that takes a sequence of tokens and produces an output of type `Output`.
 *
 * @tparam Output the type of the output produced by the parser.
 */
type TokenParser[Output] = Parser[Tokens, Output]

/** A partial implementation of an interpreter of [[ParserAlgebra]] for [[TokenParser]]. */
trait TokenParserInterpreter extends ParserInterpreter[Tokens] {
  override def literal(expected: String): TokenParser[String] = {
    input => {
      Try {
        if !input.startsWith(expected.asTokens) then
          throw ParserError(input, s"Expected $expected at this position $input.")
        input.splitAt(expected.length) match {
          case (matched, remaining) => (remaining, matched.mkString)
        }
      }
    }
  }

  override def regex(expected: Regex): TokenParser[String] = {
    input => {
      Try {
        expected.findPrefixMatchOf(input.mkString) match {
          case None => throw ParserError(input, s"Expected a matching for ${expected.regex} at this position $input.")
          case Some(m) => (input.mkString.substring(m.end), input.mkString.substring(m.start, m.end))
        }
      }
    }
  }
}
