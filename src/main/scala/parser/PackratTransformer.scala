package neocheck
package parser

import scala.compiletime.asMatchable
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

  override def recursive[Outputs <: NamedTuple.AnyNamedTuple]
                        (p: NamedTuple.Map[Outputs, PackratParserF[Input]] => NamedTuple.Map[Outputs, PackratParserF[Input]])
                        (using size: ValueOf[NamedTuple.Size[Outputs]]): NamedTuple.Map[Outputs, PackratParserF[Input]] = {

    def packratParser(t: NamedTuple.Map[Outputs, ParserF[Input]]): NamedTuple.Map[Outputs, PackratParserF[Input]] = {
      type Packrat[T] = T match {
        case Parser[input, output] => PackratParser[input, output]
      }

      def packrat[T](t: T): Packrat[T] = {
        t.asMatchable match {
          case parser: Parser[_, _] => PackratParser(parser)
        }
      }

      // cast safety:
      // `ParserF` in T makes `Packrat[T]` produce `PackratParser`
      // `Outputs` in T is the same as `Outputs` in the cast type
      // `Input` in T is the same as `Input` in the cast type
      t.map([T] => (t: T) => packrat(t)).asInstanceOf[NamedTuple.Map[Outputs, PackratParserF[Input]]]
    }

    packratParser(underlying.recursive[Outputs](selfs => p(packratParser(selfs))))
  }

  extension [Output](self: PackratParser[Input, Output]) {
    override def flatMap[Mapped](f: Output => PackratParser[Input, Mapped]): PackratParser[Input, Mapped] = {
      PackratParser(underlying.flatMap(self)(f))
    }

    override def orElse[Else](other: PackratParser[Input, Else]): PackratParser[Input, Output | Else] = {
      PackratParser(underlying.orElse(self)(other))
    }
  }
}
