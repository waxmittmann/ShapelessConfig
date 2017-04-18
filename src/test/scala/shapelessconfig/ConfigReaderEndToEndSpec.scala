package shapelessconfig

import org.specs2.Specification
import shapeless.{ HNil, :: }
import shapelessconfig.ConfigParserBuilder.\
import shapelessconfig.MapConfigParserBuilderCompanion.{int, string}

import shapelessconfig.ConfigParserBuilderOps._
import shapelessconfig.MapConfig._

class ConfigReaderEndToEndSpec extends Specification {

  def is =
    s2"""
       |ConfigReader $shouldReadDbConfig
       |ConfigReader $shouldReadLogConfig
       |ConfigReader $shouldReadMergedConfig
    """.stripMargin

  val dbPartBuilder: ConfigParserBuilder.Aux[INPUT, DBConfig :: HNil] =
    \("db") {
      string("user") +
      string("pass") +
      string("url")
    }.to[DBConfig]

  val logPartBuilder: ConfigParserBuilder.Aux[INPUT, LogConfig :: HNil] =
    \("log") {
      string("file") +
      int("level")
    }.to[LogConfig]

  case class DBConfig(user: String, pass: String, url: String)
  case class LogConfig(file: String, level: Int)

  case class AllConfig(db: DBConfig, log: LogConfig)

  def shouldReadDbConfig = {
    val dbConfig = dbPartBuilder.read(
      Map(
        "db" ->
          Map(
            "user"  -> "Harry",
            "pass"  -> "Schmoe",
            "url"   -> "http://www.goo.com"
          )
      )
    )

    dbConfig shouldEqual(Right(DBConfig("Harry", "Schmoe", "http://www.goo.com") :: HNil))
  }

  def shouldReadLogConfig = {
    val logConfig = logPartBuilder.read(
      Map(
        "log" ->
          Map(
            "file"  -> "blah.log",
            "level" -> "3"
          )
      )
    )

    logConfig shouldEqual(Right(LogConfig("blah.log", 3) :: HNil))
  }

  def shouldReadMergedConfig = {
    val mergedReader =
       \("base")(
         dbPartBuilder +
         logPartBuilder
       ).toConfigReader[AllConfig]

    val in =
      Map("base" ->
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
      )

    mergedReader.read(in) shouldEqual(Right(
      AllConfig(
        DBConfig("Harry", "Schmoe", "http://www.goo.com"),
        LogConfig("blah.log", 3)
      )
    ))
  }
}
