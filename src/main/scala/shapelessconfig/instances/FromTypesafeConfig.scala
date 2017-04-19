package shapelessconfig.instances

import scala.util.Try

import com.typesafe.config.Config
import shapelessconfig.{Err, FromConfig}

/**
  * Components for reading config from nested maps.
  *
  * To use, 'import FromMapConfig._'
  */
object FromTypesafeConfig extends FromConfig[Config] {

  implicit val intFromInput: (INPUT, INPUT_SELECT) => Either[Err, Int] =
    (in, key) => Try(in.getInt(key)).toEither.left
      .map(t => Err(s"Failed to parse to int: ${t.getMessage}"))

  implicit val stringFromInput: (INPUT, INPUT_SELECT) => Either[Err, String] =
    (in, key) => Try(in.getString(key)).toEither.left
      .map(t => Err(s"Failed to parse to int: ${t.getMessage}"))

  implicit def narrow(in: INPUT, key: INPUT_SELECT): Either[Err, INPUT] =
    Try(in.getConfig(key)).toEither.left
      .map(t => Err(s"Failed to select subconfig $key: ${t.getMessage}"))
}
