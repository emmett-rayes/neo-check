package neocheck
package parser

/** The base types of parsers that can be defined in the parser algebra. */
enum ParserKind {
  case Literal()
  case Regex()
}
