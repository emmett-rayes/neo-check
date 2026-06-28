package neocheck
package parser

import scala.Tuple.Size

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

  override def recursive[Outputs <: Tuple](p: Tuple.Map[Outputs, ParserF[Input]] => Tuple.Map[Outputs, ParserF[Input]])
                                          (using size: ValueOf[Size[Outputs]]): Tuple.Map[Outputs, ParserF[Input]] = {
    lazy val rec: Tuple.Map[Outputs, ParserF[Input]] = {
      val parsers = Array.tabulate[Parser[Input, ?]](size.value) {
        i => {
          input => p(rec).productElement(i).asInstanceOf[Parser[Input, ?]].parse(input)
        }
      }
      Tuple.fromArray(parsers).asInstanceOf[Tuple.Map[Outputs, ParserF[Input]]]
    }
    rec
  }
}
