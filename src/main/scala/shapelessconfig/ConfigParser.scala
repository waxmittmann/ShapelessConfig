package shapelessconfig

trait ConfigParser[IN, CONFIG] {
  def read(in: IN): Either[Err, CONFIG]
}
