package shapelessconfig

import scala.util.Try

import shapeless.{::, HList, HNil}
import cats.syntax.all._

/**
  * Components for reading config from nested maps.
  *
  * To use, 'import MapConfig._'
  */
case class MapConfigParserBuilder[S](key: String, t: String => Either[Err, S]) extends ConfigParserBuilder[Map[String, _]] {
  type OUT = S :: HNil

  override def read(in: Map[String, _]): Either[Err, OUT] =
    for {
      rv   <- in.get(key).toRight(Err(s"Map did not contain $key"))

      _ <- (rv != null && rv.isInstanceOf[String]).some.toRight(Err("Retrieved value was not a string"))

      // must work because above
      v = rv.asInstanceOf[String]

      cv  <- t(v)
    } yield {
      println(cv)
      HList(cv)
    }
}

trait MapConfigParserBuilderSyntax {
  def unit[S](key: String, t: String => Either[Err, S]): MapConfigParserBuilder[S] { type OUT = S :: HNil } =
    MapConfigParserBuilder(key, t)

  def int(key: String): MapConfigParserBuilder[Int] { type OUT = Int :: HNil } =
    MapConfigParserBuilder(key, i => Try(i.toInt).toEither.left.map(t => Err(s"Failed to parse to int: ${t.getMessage}")))

  def string(key: String): MapConfigParserBuilder[String] { type OUT = String :: HNil } =
    MapConfigParserBuilder(key, Right(_))

  implicit def narrow(in: Map[String, _], key: String): Either[Err, Map[String, _]] =
    for {
      v <- in.get(key).toRight(Err(s"Key $key not found in $in"))
      newI <- Try(v.asInstanceOf[Map[String, _]]).toEither.left
                .map(t => Err(s"Failed to cast ${v.getClass} to Map[String, _].\n${t.getMessage}"))
    } yield
      newI
}

object FromMapConfig extends MapConfigParserBuilderSyntax {
  type INPUT_SELECT = String
  type INPUT = Map[INPUT_SELECT, _]

  implicit val configParserBuilder = MapConfigParserBuilder
}
