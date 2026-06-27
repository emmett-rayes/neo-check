package neocheck
package parser

import scala.util.matching.Regex

/** An interpreter transformer that lifts any interpreter of [[ParserAlgebra]] for [[Parser]] into one for [[PackratParser]],
 * decorating every constructed parser with memoization.
 *
 * @tparam Input the type of the input to be parsed.
 */
trait PackratTransformer[Input](val underlying: ParserAlgebra[ParserF[Input]])
  extends ParserAlgebra[PackratParserF[Input]] {

  override type Output[K <: ParserKind] = underlying.Output[K]

  override def literal(expected: String): PackratParser[Input, Output[ParserKind.Literal]] =
    PackratParser(underlying.literal(expected))

  override def regex(expected: Regex): PackratParser[Input, Output[ParserKind.Regex]] =
    PackratParser(underlying.regex(expected))

  override def success[Output](output: Output): PackratParser[Input, Output] =
    PackratParser(underlying.success(output))

  override def failure[Output](message: String): PackratParser[Input, Output] =
    PackratParser(underlying.failure(message))

  override def recursive[Output](p: PackratParser[Input, Output] => PackratParser[Input, Output]): PackratParser[Input, Output] =
    PackratParser(underlying.recursive(self => p(PackratParser(self))))

  extension [Output](self: PackratParser[Input, Output]) {
    override def flatMap[Mapped](f: Output => PackratParser[Input, Mapped]): PackratParser[Input, Mapped] = {
      PackratParser(underlying.flatMap(self)(f))
    }

    override def orElse[Else](other: PackratParser[Input, Else]): PackratParser[Input, Output | Else] = {
      PackratParser(underlying.orElse(self)(other))
    }
  }
}
