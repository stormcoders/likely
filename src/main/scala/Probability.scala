class Probability(v: Double) {
  val logValue = math.log(v)
  def value = math.exp(logValue)
}