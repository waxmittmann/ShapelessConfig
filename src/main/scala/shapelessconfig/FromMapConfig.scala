package shapelessconfig

import scala.util.Try

import shapeless.{::, HNil}
import cats.syntax.all._
import shapelessconfig.FromConfig

/**
  * Components for reading config from nested maps.
  *
  * To use, 'import FromMapConfig._'
  */
object FromMapConfig extends FromConfig[Map[String, _]] {

  def readFromMap(in: INPUT, key: INPUT_SELECT): Either[Err, String] =
    for {
      rv   <- in.get(key).toRight(Err(s"Map did not contain $key"))

      _ <- (rv != null && rv.isInstanceOf[String]).some.toRight(Err("Retrieved value was not a string"))

      // must work because above
      v = rv.asInstanceOf[String]
    } yield
      v

  def int(key: String): ConfigParserBuilderInstance[Int] { type OUT = Int :: HNil } =
    ConfigParserBuilderInstance[Int](
      key,
      (in, key) => readFromMap(in, key)
                .flatMap(s => Try(s.toInt).toEither.left
                .map(t => Err(s"Failed to parse to int: ${t.getMessage}")))
    )

  def string(key: String): ConfigParserBuilderInstance[String] { type OUT = String :: HNil } =
    ConfigParserBuilderInstance[String](
      key,
      (in, key) => readFromMap(in, key)
    )

  implicit def narrow(in: Map[String, _], key: String): Either[Err, Map[String, _]] =
    for {
      v <- in.get(key).toRight(Err(s"Key $key not found in $in"))
      newI <- Try(v.asInstanceOf[Map[String, _]]).toEither.left
        .map(t => Err(s"Failed to cast ${v.getClass} to Map[String, _].\n${t.getMessage}"))
    } yield
      newI
}
