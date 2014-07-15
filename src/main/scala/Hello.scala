/**
 * Created by Yoann Canal on 10/07/14.
 */

import java.io.File

import model._
import model.parser.EOEntityParser

object Main extends App {
  def printAsPb(entity: YNPEntity) {
    var i = 0;
    (for {
      YNPAttribute(name, ynpType, additions) <- entity.attributes
    } yield {
      i += 1
      "   optional " + name + " " + (
        ynpType match {
          case YNPBoolean => "boolean"
          case YNPInt => "int32"
          case YNPString => "string"
          case YNPTimestamp => "int64"
        }) + " = " + i + ";"
    }).foreach((s: String) => println(s))

    println("}")
  }
  val path = new File(args(0))
  if (!path.exists) {
    println("accepts only an EOModel or an EOEntity.plist as arg")
    sys.exit(1)
  }

  if (path.isFile && path.getName.endsWith(".plist")) {
    val entity = EOEntityParser.parse(path)
    println("message " + entity.name + " {")
    printAsPb(entity)
  } else if (path.isDirectory) {
    for {file <- path.listFiles.toIterator if file.isFile && file.getName.endsWith(".plist")}
      try {
        val entity = EOEntityParser.parse(file)
        println(file.getName, entity)
        printAsPb(entity)
        println()
      } catch {
        case e: Exception =>
          println("failed to parse " + file.getName)
          e.printStackTrace()
      }
  } else {
    println("accepts only an EOModel or an EOEntity.plist as arg")
    sys.exit(1)
  }
}
