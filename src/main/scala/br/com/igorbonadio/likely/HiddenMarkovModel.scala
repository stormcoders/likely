package br.com.igorbonadio.likely

class HiddenMarkovModel(states: Alphabet, symbols: Alphabet, emissions: Map[(Int, Int), LogProbability], transitions: Map[(Int, Int), LogProbability]) {
  def prob(x: Stream[Int], y: Stream[Int]) = {
    def prob2(xy: Stream[(Int, Int)], yprev: Int): LogProbability = xy match {
      case Stream() => Probability(1)
      case (x, y) #:: xys => emissions(x,y) * transitions(y, yprev) * prob2(xys, y)
    }
    prob2(x zip y, 0)
  }
}