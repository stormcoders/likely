class IIDModel(distribution: DiscreteDistribution) {
  def prob(sequence: List[Int]):LogProbability = sequence match {
    case x::xs => distribution.prob(x) * prob(xs)
    case List() => Probability(1)
  }
}