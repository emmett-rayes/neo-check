package neocheck
package parser

import scala.util.Try
import scala.util.matching.Regex

/** A parser that takes a sequence of tokens and produces an output of type `Output`.
 *
 * @tparam Output the type of the output produced by the parser.
 */
type TokenParser[Output] = Parser[Tokens, Output]

/** An interpreter of [[ParserAlgebra]] for [[TokenParser]]. */
trait TokenParserInterpreter extends ParserInterpreter[Tokens] {
  override type Output[K <: ParserKind] = K match {
    case ParserKind.Literal => String
    case ParserKind.Regex => String
  }

  override def literal(expected: String): TokenParser[String] = {
    input => {
      val trimmed = input.skipWhitespace
      Try {
        if !trimmed.startsWith(expected.asTokens) then
          throw ParserError(input, s"Expected $expected at this position $trimmed.")
        trimmed.splitAt(expected.length) match {
          case (matched, remaining) => (remaining, matched.mkString)
        }
      }
    }
  }

  override def regex(expected: Regex): TokenParser[String] = {
    input => {
      val trimmed = input.skipWhitespace
      Try {
        if trimmed.isEmpty then
          throw ParserError(input, "Expected input at this position.")
        expected.findPrefixMatchOf(trimmed.mkString) match {
          case None => throw ParserError(input, s"Expected a matching for ${expected.regex} at this position $trimmed.")
          case Some(m) => (trimmed.mkString.substring(m.end), trimmed.mkString.substring(m.start, m.end))
        }
      }
    }
  }
}
