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
  override def read[S](in: Map[String, _], key: String, t: String => Either[Err, S]): Either[Err, S] =
    for {
      rv   <- in.get(key).toRight(Err(s"Map did not contain $key"))

      _ <- (rv != null && rv.isInstanceOf[String]).some.toRight(Err("Retrieved value was not a string"))

      // must work because above
      v = rv.asInstanceOf[String]

      cv  <- t(v)
    } yield
      cv

  def unit[S](key: String, t: String => Either[Err, S]): ConfigParserBuilderInstance[S] { type OUT = S :: HNil } =
    ConfigParserBuilderInstance[S](key, t)

  def int(key: String): ConfigParserBuilderInstance[Int] { type OUT = Int :: HNil } = {
    ConfigParserBuilderInstance[Int](key, i => Try(i.toInt).toEither.left.map(t => Err(s"Failed to parse to int: ${t.getMessage}")))
  }

  def string(key: String): ConfigParserBuilderInstance[String] { type OUT = String :: HNil } =
    ConfigParserBuilderInstance[String](key, Right(_))

  implicit def narrow(in: Map[String, _], key: String): Either[Err, Map[String, _]] =
    for {
      v <- in.get(key).toRight(Err(s"Key $key not found in $in"))
      newI <- Try(v.asInstanceOf[Map[String, _]]).toEither.left
        .map(t => Err(s"Failed to cast ${v.getClass} to Map[String, _].\n${t.getMessage}"))
    } yield
      newI
}
