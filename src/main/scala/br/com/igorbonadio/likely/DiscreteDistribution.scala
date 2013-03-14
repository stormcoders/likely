package br.com.igorbonadio.likely

class DiscreteDistribution(probs: List[LogProbability]) extends Distribution[Int] {
  val probabilities = probs
  val probabilityMap = ((0 to (probabilities.length - 1)) zip probabilities).toMap
  
  def prob(id: Int):LogProbability = probabilityMap(id)
}