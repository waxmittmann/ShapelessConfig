package shapelessconfig

import ConfigParserBuilder.\
import ConfigParserBuilderOps.ConfigParserBuilderOpsTc
import MapConfigParserBuilderCompanion._

object Main {

  def main(args: Array[String]): Unit = {
    case class DBConfig(user: String, pass: String, url: String)
    case class AllDbConfig(db: DBConfig)

    case class LogConfig(file: String, level: Int)
    case class AllConfig(db: DBConfig, log: LogConfig)

    import MapConfigParserBuilderCompanion._
    implicit val m = NarrowMapInput.m(_, _)

    if (true) {

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
        }.toConfigReader[AllConfig]

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

      val all = \("base") { dbPart + logPart }.toConfigReader[AllConfig]

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
  }




}
