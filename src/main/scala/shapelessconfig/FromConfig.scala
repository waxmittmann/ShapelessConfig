package shapelessconfig

import shapeless.{::, HNil}

trait FromConfig[I] { self =>
  type INPUT_SELECT = String
  type INPUT = I

  case class ConfigParserBuilderInstance[S](key: INPUT_SELECT, t: (INPUT, INPUT_SELECT) => Either[Err, S]) extends ConfigParserBuilder[INPUT] {
    type OUT = S :: HNil

    override def read(in: INPUT): Either[Err, OUT] =
      t(in, key).map(_ :: HNil)
  }

  def instance[S](key: INPUT_SELECT, t: (INPUT, INPUT_SELECT) => Either[Err, S]) =
    ConfigParserBuilderInstance(key, t)

  implicit val intFromInput: (INPUT, INPUT_SELECT) => Either[Err, Int]

  implicit val stringFromInput: (INPUT, INPUT_SELECT) => Either[Err, String]

  implicit def narrow(in: INPUT, key: INPUT_SELECT): Either[Err, INPUT]
}