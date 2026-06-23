package neocheck
package parser

import scala.util.{Failure, Success, Try}

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

/** A partial implementation of an interpreter of [[ParserAlgebra]] for [[Parser]].
 *
 * @tparam Input the type of the input to be parsed.
 */
abstract class ParserInterpreter[Input] extends ParserAlgebra[[Output] =>> Parser[Input, Output]] {
  override def success[Output](output: Output): Parser[Input, Output] = {
    input => Success((input, output))
  }

  override def failure[Output](message: String): Parser[Input, Output] = {
    input => Failure(ParserError(input, message))
  }

  extension [Output](self: => Parser[Input, Output]) {
    override def flatMap[Mapped](f: Output => Parser[Input, Mapped]): Parser[Input, Mapped] = {
      input =>
        self.parse(input).flatMap((remaining, output) =>
          Try {
            f(output).parse(remaining) match {
              case Failure(exception) => throw ParserError(input, message = exception.getMessage, cause = exception)
              case Success(result) => result
            }
          }
        )
    }

    override def orElse[Else](other: => Parser[Input, Else]): Parser[Input, Output | Else] = {
      input => self.parse(input).orElse(other.parse(input))
    }
  }
}
