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

object MapConfigParserBuilderCompanion {
  def unit[S](key: String, t: String => Either[Err, S]): MapConfigParserBuilder[S] { type OUT = S :: HNil } =
    MapConfigParserBuilder(key, t)

  def int(key: String): MapConfigParserBuilder[Int] { type OUT = Int :: HNil } =
    MapConfigParserBuilder(key, i => Try(i.toInt).toEither.left.map(t => Err(s"Failed to parse to int: ${t.getMessage}")))

  def string(key: String): MapConfigParserBuilder[String] { type OUT = String :: HNil } =
    MapConfigParserBuilder(key, Right(_))
}

object NarrowMapInput {
  implicit def m(in: Map[String, _], key: String): Either[Err, Map[String, _]] =
    for {
      v <- in.get(key).toRight(Err(s"Key $key not found in $in"))
      _ <- (v.isInstanceOf[Map[String, _]]).some.toRight(Err("Is not a map"))
      newI = v.asInstanceOf[Map[String, _]]
    } yield
      newI
}

object MapConfig {
  type INPUT_SELECT = String
  type INPUT = Map[INPUT_SELECT, _]

  implicit val narrowMapInput = NarrowMapInput.m _

  implicit val configParserBuilderCompanion = MapConfigParserBuilderCompanion

  implicit val configParserBuilder  = MapConfigParserBuilder
}