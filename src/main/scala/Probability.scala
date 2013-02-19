class LogProbability(v: Double) {
  val logValue = v
  def value = math.exp(-logValue)
  
  def *(p: LogProbability):LogProbability =
    new LogProbability(logValue + p.logValue)
    
  def /(p: LogProbability):LogProbability =
    new LogProbability(logValue - p.logValue)
}

object Probability {
  def apply(v: Double) = new LogProbability(-math.log(v))
}