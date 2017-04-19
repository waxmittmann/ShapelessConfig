package shapelessconfig.instances

import scala.util.Try

import com.typesafe.config.Config
import shapeless.{::, HNil}
import shapelessconfig.{Err, FromConfig}

/**
  * Components for reading config from nested maps.
  *
  * To use, 'import FromMapConfig._'
  */
object FromTypesafeConfig extends FromConfig[Config] {

  def int(key: String): ConfigParserBuilderInstance[Int] { type OUT = Int :: HNil } =
    ConfigParserBuilderInstance[Int](
      key,
      (in, key) => Try(in.getInt(key)).toEither.left
                    .map(t => Err(s"Failed to parse to int: ${t.getMessage}"))
    )

  def string(key: String): ConfigParserBuilderInstance[String] { type OUT = String :: HNil } =
    ConfigParserBuilderInstance[String](
      key,
      (in, key) => Try(in.getString(key)).toEither.left
                    .map(t => Err(s"Failed to parse to int: ${t.getMessage}"))
    )

  implicit def narrow(in: INPUT, key: INPUT_SELECT): Either[Err, INPUT] =
    Try(in.getConfig(key)).toEither.left
      .map(t => Err(s"Failed to select subconfig $key: ${t.getMessage}"))
}
