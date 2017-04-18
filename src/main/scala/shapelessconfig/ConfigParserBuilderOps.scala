package shapelessconfig

import shapeless.ops.hlist.Prepend
import shapeless.HList

object ConfigParserBuilderOps {
  def <+>[IN, O <: HList](a: ConfigParserBuilder[IN])(b: ConfigParserBuilder[IN])(implicit prep: Prepend.Aux[a.OUT, b.OUT, O]): ConfigParserBuilder.Aux[IN, O] =
    new ConfigParserBuilder[IN] {
      type OUT = prep.Out

      override def read(in: IN): Either[Err, O] =
        for {
          i1 <- a.read(in)
          i2 <- b.read(in)
        } yield
          prep(i1, i2)
    }

  def -->[IN, U, O <: HList](a: ConfigParserBuilder[IN])(transformerInput: U)(b: ConfigParserBuilder[IN])(implicit prep: Prepend.Aux[a.OUT, b.OUT, O], inTransformer: (IN, U) => Either[Err, IN]): ConfigParserBuilder.Aux[IN, O] =
    new ConfigParserBuilder[IN] {
      type OUT = prep.Out

      override def read(in: IN): Either[Err, O] =
        for {
          i1    <- a.read(in)
          newIn <- inTransformer(in, transformerInput)
          i2    <- b.read(newIn).left.map(err => Err(s"/$inTransformer ${err.msg}"))
        } yield
          prep(i1, i2)
    }

  implicit class ConfigParserBuilderOpsTc[IN, S <: HList, T <: HList, O <: HList](a: ConfigParserBuilder.Aux[IN, S]) {
    def +(b: ConfigParserBuilder.Aux[IN, T])(implicit prep: Prepend.Aux[S, T, O]): ConfigParserBuilder.Aux[IN, O] =
      ConfigParserBuilderOps.<+>(a)(b)(prep)

    def -->[U](transformerInput: U)(b: ConfigParserBuilder.Aux[IN, T])(implicit prep: Prepend.Aux[S, T, O], inTransformer: (IN, U) => Either[Err, IN]): ConfigParserBuilder.Aux[IN, O] =
      ConfigParserBuilderOps.-->(a)(transformerInput)(b)(prep, inTransformer)
  }
}