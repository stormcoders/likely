package br.com.igorbonadio.likely

import Fancy._

class HiddenMarkovModel(emissions: Map[Int, DiscreteDistribution], transitions: Map[Int, DiscreteDistribution]) {
  def prob(x: Stream[Int], y: Stream[Int]) = {
    def prob2(xy: Stream[(Int, Int)], yprev: Int): LogProbability = xy match {
      case Stream() => 100.0.%%
      case (x, y) #:: xys => emissions(y).prob(x) * transitions(yprev).prob(y) * prob2(xys, y)
    }
    prob2(x zip y, 0)
  }
}

object HiddenMarkovModel {
  def apply(symbols: Alphabet, states: Alphabet)(definition: (HiddenMarkovModelDefinition => Unit) ) = {
    val hmmDefinition = new HiddenMarkovModelDefinition(symbols, states)
    definition(hmmDefinition)
    hmmDefinition.hmm
  }
}

class HiddenMarkovModelDefinition(symbols: Alphabet, states: Alphabet) {
  var emissionsMap: Map[Int, DiscreteDistribution] = null
  var transitionsMap: Map[Int, DiscreteDistribution] = null

  def transitions(definition: (ConditionalProbabilityOf => Unit)) = {
    emissionsMap = ConditionalProbabilities(symbols, states)(definition)
  }

  def emissions(definition: (ConditionalProbabilityOf => Unit)) = {
    transitionsMap = ConditionalProbabilities(states, states)(definition)
  }

  def hmm = new HiddenMarkovModel(emissionsMap, transitionsMap)
}