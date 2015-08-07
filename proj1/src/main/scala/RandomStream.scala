
object RandomStream{
  val modulusMask = Int.MaxValue
  val multiplier = 1103515245
  val increment = 12345

  def apply(seed: Int) = {
    lazy val s: Stream[Int] = Stream.cons(
      seed,
      s.map(x => (x * multiplier + increment) & modulusMask)
    )
    s.map(x => (x & 0x7fff0000) >> 16) // get only bits 30..16
  }
}
