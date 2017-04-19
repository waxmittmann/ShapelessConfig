package shapelessconfig

import shapeless.{::, Generic, HList, HNil}

object ConfigParserBuilder {
  type Aux[T, H] = ConfigParserBuilder[T] { type OUT = H }

  def apply[IN](): ConfigParserBuilder.Aux[IN, HNil] = {
    new ConfigParserBuilder[IN] {
      override type OUT = HNil

      override def read(in: IN): Either[Err, OUT] = Right(HNil)
    }
  }

  def \[IN, T](transformerInput: T)(other: ConfigParserBuilder[IN])(implicit inTransformer: (IN, T) => Either[Err, IN]): ConfigParserBuilder.Aux[IN, other.OUT] =
    new ConfigParserBuilder[IN] {
      override type OUT = other.OUT

      override def read(in: IN): Either[Err, other.OUT] =
        for {
          newIn <- inTransformer(in, transformerInput)
          i2    <- other.read(newIn)
        } yield
          i2
    }

  def int[IN](key: String)(implicit t: (IN, String) => Either[Err, Int]): ConfigParserBuilder.Aux[IN, Int :: HNil] { type OUT = Int :: HNil } =
    ConfigParserBuilderInstance(key, t)

  def string[IN](key: String)(implicit t: (IN, String) => Either[Err, String]): ConfigParserBuilder.Aux[IN, String :: HNil] { type OUT = String :: HNil } =
    ConfigParserBuilderInstance(key, t)

  def instance[S, INPUT](key: String, t: (INPUT, String) => Either[Err, S]) =
    ConfigParserBuilderInstance(key, t)

  case class ConfigParserBuilderInstance[S, INPUT](key: String, t: (INPUT, String) => Either[Err, S]) extends ConfigParserBuilder[INPUT] {
    type OUT = S :: HNil

    override def read(in: INPUT): Either[Err, OUT] =
      t(in, key).map(_ :: HNil)
  }
}

trait ConfigParserBuilder[IN] { self =>
  type OUT <: HList

  def read(in: IN): Either[Err, OUT]

  def readAs[FINAL](in: IN)(implicit generic: Generic.Aux[FINAL, OUT]): Either[Err, FINAL] =
    read(in).map(generic.from(_))

  def to[FINAL](implicit generic: Generic.Aux[FINAL, OUT]): ConfigParserBuilder.Aux[IN, FINAL :: HNil] =
    new ConfigParserBuilder[IN] {
      type OUT = FINAL :: HNil

      override def read(in: IN): Either[Err, OUT] =
        self.readAs[FINAL](in).map(_ :: HNil)
    }

  def toConfigParser[FINAL](implicit generic: Generic.Aux[FINAL, OUT]): ConfigParser[IN, FINAL] =
    (in: IN) => readAs[FINAL](in)
}
