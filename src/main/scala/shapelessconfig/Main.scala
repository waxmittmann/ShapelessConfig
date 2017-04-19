package shapelessconfig

import ConfigParserBuilder.\
import ConfigParserBuilderOps.ConfigParserBuilderOpsTc
import com.typesafe.config.{Config, ConfigFactory}
import scala.collection.JavaConverters._
import java.util

object Main {

  def main(args: Array[String]): Unit = {
    case class DBConfig(user: String, pass: String, url: String)
    case class AllDbConfig(db: DBConfig)

    case class LogConfig(file: String, level: Int)
    case class AllConfig(db: DBConfig, log: LogConfig)

    if (false) {
      import shapelessconfig.instances.FromMapConfig._
      import shapelessconfig.instances.FromMapConfig.{int, string}

      val asOneBigBlob: ConfigParser[Map[String, _], AllConfig] =
        \("base") {
          \("db") {
            string("user") +
              string("pass") +
              string("url")
          }.to[DBConfig] +
            \("log") {
              string("file") +
                int("level")
            }.to[LogConfig]
        }.toConfigParser[AllConfig]

      /*
       Note / Learning:
       If I give dbPart a type signature that doesn't include the {type TOUT = BLAH}
         part then ConfigParserBuilderOpsTc(..) case class won't work, because it's
         missing the {type TOUT = ...} part!

       So careful with type signatures. Should create aux type.
      */
      val dbPart =
      \("db") {
        string("user") +
          string("pass") +
          string("url")
      }.to[DBConfig]

      val logPart = \("log") {
        string("file") +
          int("level")
      }.to[LogConfig]

      val all = \("base") { dbPart + logPart }.toConfigParser[AllConfig]

      println(all.read(Map("base" ->
        //      println(asOneBigBlob.reader(Map("base" ->
        Map(
          "db" ->
            Map(
              "user"  -> "Harry",
              "pass"  -> "Schmoe",
              "url"   -> "http://www.goo.com"
            ),
          "log" ->
            Map(
              "file"  -> "blah.log",
              "level" -> "3"
            )
        )
      )))
    }


    if (true) {
      import shapelessconfig.instances.FromTypesafeConfig._

      val dbPart =
        \("db") {
          string("user") +
            string("pass") +
            string("url")
        }.to[DBConfig]

      val logPart = \("log") {
        string("file") +
          int("level")
      }.to[LogConfig]

      val all = \("base") { dbPart + logPart }.toConfigParser[AllConfig]

//      val config = ConfigFactory.parseMap(
//       Map("base" ->
//         Map(
//           "db" ->
//             Map(
//               "user"  -> "Harry",
//               "pass"  -> "Schmoe",
//               "url"   -> "http://www.goo.com"
//             ),
//           "log" ->
//             Map(
//               "file"  -> "blah.log",
//               "level" -> "3"
//             )
//         )
//       ).asJava
//      )

      val base = new util.HashMap[String, Object]()

      val base2 = new util.HashMap[String, Object]()
      base.put("base", base2)

      val db = new util.HashMap[String, Object]()
      base2.put("db", db)
      db.put("user", "Harry")
      db.put("pass", "Schmoe")
      db.put("url", "www.google.com")

      val log = new util.HashMap[String, Object]()
      base2.put("log", log)
      log.put("file", "blah.log")
      log.put("level", new Integer(3))

      val config = ConfigFactory.parseMap(base)

      println(all.read(config))
    }
  }
}
