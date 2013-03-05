package br.com.igorbonadio.likely

class DiscreteDistribution(probs: List[LogProbability]) {
  val probabilities = probs
  val probbabilityMap = ((0 to (probabilities.length - 1)) zip probabilities).toMap
  
  def prob(id: Int):LogProbability = probbabilityMap(id)
}