package model

abstract class YNPEntityAddition
abstract class YNPAttributeAddition

case class YNPAttribute(val name: String, val ynpType: YNPType, val additions: Map[String, YNPAttributeAddition])

sealed abstract class YNPType
case object YNPBoolean extends YNPType
case object YNPInt extends YNPType
case object YNPString extends YNPType
case object YNPTimestamp extends YNPType

class YNPEntity(val name: String, val attributes: List[YNPAttribute], val additions: Map[String, YNPEntityAddition])
