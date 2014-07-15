package model.parser

import java.io.File

import model._

import scala.collection.immutable.{List, Set}


sealed abstract class EOType
case object EOAmount extends EOType
case object EOBlob extends EOType
case object EOBoolean extends EOType
case object EODate extends EOType
case object EODateTime extends EOType
case object EODouble extends EOType
case object EOId extends EOType
case object EOInt extends EOType
case object EOIpAddress extends EOType
case object EOEnum extends EOType
case object EOLongNumber extends EOType
case object EONSMutableDictionary extends EOType
case object EOOsType extends EOType
case class  EOVarChar(size: Int) extends EOType
case object EOText extends EOType

case class EOAttributeAddition(columnName: String, eoType: EOType) extends YNPAttributeAddition
case class EOEntityAddition(className: String, externalName: String, primaryKeyAttributes: List[String], isReadOnly: Option[Boolean], userInfo: Option[Map[String, String]]) extends YNPEntityAddition

class CC[T] { def unapply(a:Any):Option[T] = Some(a.asInstanceOf[T]) }
object L  extends CC[List[Any]]
object LS extends CC[List[String]]
object MS extends CC[Map[String, String]]
object S  extends CC[String]

object EOEntityParser {
  def parseBoolean(input: Option[Any]): Option[Boolean] = {
    input match {
      case Some("Y") => Some(true)
      case Some("N") => Some(false)
      case _         => None
    }
  }

  def parseAttribute(input: Map[String, Any]): YNPAttribute = {
    val S(name) = input("name")
    val S(prototypeName) = input("prototypeName")
    val S(columnName) = input("columnName")
    val allowsNull = parseBoolean(input.get("allowsNull"))
    // TODO use it
    // val userInfo = input.get("userInfo").asInstanceOf[Option[Map[String, String]]]
    val eoType = prototypeName match {
      case "amount" => EOAmount
      case "blob" => EOBlob
      case "boolean" => EOBoolean
      case "date" => EODate
      case "dateTime" => EODateTime
      case "doubleNumber" => EODouble
      case "id" | "intNumber" => EOInt
      case "ipAddress" => EOIpAddress
      case "javaEnum" => EOEnum
      case "longNumber" => EOLongNumber
      case "longText" => EOText
      case "mutableDictionary" => EONSMutableDictionary
      case "osType" => EOOsType
      case "varcharLarge" => EOText
      case _ if prototypeName.startsWith("varchar") =>
        val length = prototypeName.substring("varchar".length).toInt
        EOVarChar(length)
      case _ => throw new IllegalArgumentException("unknown type \"" + input + "\"")
    }

    YNPAttribute(name, YNPInt, Map() + (("eo", EOAttributeAddition(columnName, eoType))))
  }

  def parse(file: File): YNPEntity = {
    val plist = PlistParser.parse(file).get

    val classProperties: Set[String] =
      plist.get("classProperties") match {
        case Some(LS(l)) => Set(l: _*)
        case None => Set()
      }

    val L(inAttributes) = plist("attributes")
    // TODO handle non property attribute
    val attributes = for {
      MS(attribute) <- inAttributes if classProperties.contains(attribute("name"))
    } yield {
      parseAttribute(attribute)
    }


    val S(className) = plist("className")
    val S(externalName) = plist("externalName")
    val S(name) = plist("name")
    val LS(primaryKeyAttributes) = plist("primaryKeyAttributes")

    // TODO always empty in our models, ignore ?
    // val fetchSpecificationDictionary = plist.get("fetchSpecificationDictionary").asInstanceOf[Option[Map[String, Any]]]
    val isReadOnly = parseBoolean(plist.get("isReadOnly"))

    // TODO not implemented
    // val relationships = plist.get("relationships").asInstanceOf[Option[List[Map[String, Any]]]]
    val userInfo = plist.get("userInfo").asInstanceOf[Option[Map[String, String]]]

    val additions = Map[String, YNPEntityAddition]() + (("eo", new EOEntityAddition(className, externalName, primaryKeyAttributes, isReadOnly, userInfo)))
    new YNPEntity(name, attributes, additions)
  }
}
