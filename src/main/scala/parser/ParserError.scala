package neocheck
package parser

/** An error that occurred during parsing.
 *
 * @param input   the input that caused the error.
 * @param message an optional error message.
 * @param cause   an optional cause of the error.
 * @tparam Input the type of the input that caused the error.
 */
case class ParserError[Input](input: Input, message: String | Null = null, cause: Throwable | Null = null)
  extends Exception(message, cause)
