package br.com.igorbonadio.likely

class HiddenMarkovModel(emissions: Map[Int, DiscreteDistribution], transitions: Map[Int, DiscreteDistribution]) {
  def prob(x: Stream[Int], y: Stream[Int]) = {
    def prob2(xy: Stream[(Int, Int)], yprev: Int): LogProbability = xy match {
      case Stream() => Probability(1)
      case (x, y) #:: xys => emissions(y).prob(x) * transitions(yprev).prob(y) * prob2(xys, y)
    }
    prob2(x zip y, 0)
  }
}