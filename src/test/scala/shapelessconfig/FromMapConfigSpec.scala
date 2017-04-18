package shapelessconfig

import org.specs2.Specification

import shapeless.{ HNil, :: }
import shapelessconfig.ConfigParserBuilder.\

import shapelessconfig.FromMapConfig._

class FromMapConfigSpec extends Specification {
  override def is = s2"""
    FromMapConfig should fail correctly if input is not a map $shouldFailCorrectlyIfInputIsNotMap
      """

  def shouldFailCorrectlyIfInputIsNotMap = {
    val input = Map("a" -> "b")

    val result: Either[Err, ::[String, HNil]] =
      \("a") {
        \("b") {
          string("hello")
        }
      }.read(input)

    result should beLeft(
      (e: Err) => e.msg should startWith("Failed to cast")
    )
  }
}
