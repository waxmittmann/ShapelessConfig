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

  def int(key: String): ConfigParserBuilder[INPUT] { type OUT = Int :: HNil }

  def string(key: String): ConfigParserBuilder[INPUT] { type OUT = String :: HNil }

  implicit def narrow(in: Map[String, _], key: String): Either[Err, Map[String, _]]
}