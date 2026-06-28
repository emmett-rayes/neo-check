package neocheck
package parser

import scala.util.{Failure, Success, Try}


/** A partial implementation of an interpreter of [[ParserAlgebra]] for [[Parser]].
 *
 * @tparam Input the type of the input to be parsed.
 */
trait ParserInterpreter[Input] extends ParserAlgebra[ParserF[Input]] {
  override def success[Output](output: Output): Parser[Input, Output] = {
    input => Success((input, output))
  }

  override def failure[Output](message: String): Parser[Input, Output] = {
    input => Failure(ParserError(input, message))
  }

  extension [Output](self: Parser[Input, Output]) {
    override def not: Parser[Input, Unit] = {
      input => {
        self.parse(input) match {
          case Success(_) => Failure(ParserError(input, "Illegal input at this position."))
          case Failure(_) => Success((input, ()))
        }
      }
    }

    override def flatMap[Mapped](f: Output => Parser[Input, Mapped]): Parser[Input, Mapped] = {
      input => {
        self.parse(input).flatMap((remaining, output) =>
          Try {
            f(output).parse(remaining) match {
              case Failure(exception) => throw ParserError(input, message = exception.getMessage, cause = exception)
              case Success(result) => result
            }
          }
        )
      }
    }

    override def orElse[Else](other: Parser[Input, Else]): Parser[Input, Output | Else] = {
      input => {
        self.parse(input).orElse(other.parse(input))
      }
    }
  }
}
