package model.parser

import java.io.{File, FileReader}

import scala.collection.immutable.List
import scala.util.parsing.combinator.JavaTokenParsers

object PlistParser extends PlistParser {
  def parse(file: File): ParseResult[Map[String, Any]] = {
    parseAll(plistDictionary, new FileReader(file))
  }
}

class PlistParser extends JavaTokenParsers {
  def value: Parser[Any] = (
        plistDictionary
      | plistArray
      | plistString
    )

  def plistDictionary: Parser[Map[String, Any]] =
    "{" ~> rep(plistMember) <~ "}" ^^ (Map() ++ _)

  def plistMember: Parser[(String, Any)] =
    plistString ~ "=" ~ value ~ ";" ^^ { case name ~ "=" ~ value ~ ";" => (name, value)}

  def plistArray: Parser[List[Any]] =
    "(" ~> repsep(value, ",") <~ ")"

  def plistString: Parser[String] = (
        stringLiteral
      | """[a-zA-Z0-9]+""".r
    )
}
