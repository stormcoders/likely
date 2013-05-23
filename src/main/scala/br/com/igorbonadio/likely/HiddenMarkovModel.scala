package br.com.igorbonadio.likely

import Fancy._

class HiddenMarkovModel(emissions: Map[Int, DiscreteDistribution], transitions: Map[Int, DiscreteDistribution], initialProbabilities: DiscreteIIDModel) {
  def prob(x: Stream[Int], y: Stream[Int]) = {
    def prob2(xy: Stream[(Int, Int)], yprev: Int): LogProbability = xy match {
      case Stream() => 100.0.%%
      case (x, y) #:: xys => emissions(y).prob(x) * transitions(yprev).prob(y) * prob2(xys, y)
    }
    initialProbabilities.prob(Stream(y.head))*emissions(y.head).prob(x.head)*prob2(x.tail zip y.tail, y.head)
  }
}

object HiddenMarkovModel {
  def apply(symbols: Alphabet, states: Alphabet)(definition: (HiddenMarkovModelDefinition => Unit) ) = {
    val hmmDefinition = new HiddenMarkovModelDefinition(symbols, states)
    definition(hmmDefinition)
    hmmDefinition.hmm
  }

  class HiddenMarkovModelDefinition(symbols: Alphabet, states: Alphabet) {
    var emissionsMap: Map[Int, DiscreteDistribution] = null
    var transitionsMap: Map[Int, DiscreteDistribution] = null
    var initialProbabilitiesDistribution: DiscreteIIDModel = null

    def transitions(definition: (ConditionalProbabilityOf => Unit)) = {
      emissionsMap = ConditionalProbabilities(symbols, states)(definition)
    }

    def emissions(definition: (ConditionalProbabilityOf => Unit)) = {
      transitionsMap = ConditionalProbabilities(states, states)(definition)
    }

    def initialProbabilities(definition: (ProbabilityOf => Unit)) = {
      initialProbabilitiesDistribution = DiscreteIIDModel(states)(definition)
    }

    def hmm = new HiddenMarkovModel(emissionsMap, transitionsMap, initialProbabilitiesDistribution)
  }
}