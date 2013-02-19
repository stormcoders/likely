class Probability(v: Double) {
  val logValue = v
  def value = math.exp(logValue)
  
  def *(p: Probability):Probability =
    new Probability(logValue + p.logValue)
    
  def /(p: Probability):Probability =
    new Probability(logValue - p.logValue)
}

object Prob {
  def apply(v: Double) = new Probability(math.log(v))
}