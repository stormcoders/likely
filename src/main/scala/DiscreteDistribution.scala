class DiscreteDistribution(probabilities: List[Double]) {
  val probbabilityMap = ((0 to (probabilities.length - 1)) zip probabilities).toMap
  
  def prob(id: Int):Double = probbabilityMap(id)
}