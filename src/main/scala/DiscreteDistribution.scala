class DiscreteDistribution(probabilities: List[LogProbability]) {
  val probbabilityMap = ((0 to (probabilities.length - 1)) zip probabilities).toMap
  
  def prob(id: Int):LogProbability = probbabilityMap(id)
}