package neocheck
package parser

/** A partial implementation of an interpreter of [[ParserAlgebra]] for [[Parser]].
 *
 * Uses a naive fixpoint combinator which can lead to stack overflow on left-recursive parsers.
 *
 * @tparam Input the type of the input to be parsed.
 */
trait NaiveRecursionInterpreter[Input] extends ParserAlgebra[ParserF[Input]] {
  override def recursive[Output](p: Parser[Input, Output] => Parser[Input, Output]): Parser[Input, Output] = {
    lazy val rec: Parser[Input, Output] = input => p(rec).parse(input)
    rec
  }
}
