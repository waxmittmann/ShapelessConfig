package shapelessconfig

import shapeless.{::, HNil}

trait FromConfig[I] { self =>
  type INPUT_SELECT = String
  type INPUT = I

  def read[S](in: INPUT, key: String, t: String => Either[Err, S]): Either[Err, S]

  def unit[S](key: String, t: String => Either[Err, S]): ConfigParserBuilder[INPUT] { type OUT = S :: HNil }

  def int(key: String): ConfigParserBuilder[INPUT] { type OUT = Int :: HNil }

  def string(key: String): ConfigParserBuilder[INPUT] { type OUT = String :: HNil }

  implicit def narrow(in: Map[String, _], key: String): Either[Err, Map[String, _]]


  case class ConfigParserBuilderInstance[S](key: String, t: String => Either[Err, S]) extends ConfigParserBuilder[INPUT] {
    type OUT = S :: HNil

    override def read(in: INPUT): Either[Err, OUT] =
      self.read(in, key, t).map(_ :: HNil)
  }
}