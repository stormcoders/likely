package br.com.stormcoders.likely

import Fancy._

class HiddenMarkovModel(emissions: Map[Int, DiscreteDistribution], transitions: Map[Int, DiscreteDistribution], initialProbabilities: DiscreteIIDModel, symbols: Alphabet, states: Alphabet) {
  def prob(x: Stream[Int], y: Stream[Int]) = {
    def prob2(xy: Stream[(Int, Int)], yprev: Int): LogProbability = xy match {
      case Stream() => 100.0.%%
      case (x, y) #:: xys => emissions(y).prob(x) * transitions(yprev).prob(y) * prob2(xys, y)
    }
    initialProbabilities.prob(Stream(y.head))*emissions(y.head).prob(x.head)*prob2(x.tail zip y.tail, y.head)
  }

  def viterbi(x: Stream[Int]) = {
    var gamma = Array.ofDim[LogProbability](states.size, x.size)
    var psi = Array.ofDim[Int](states.size, x.size)

    for (k <- 0 until states.size) {
      gamma(k)(0) = initialProbabilities.prob(Stream(k)) * emissions(k).prob(x.head)
    }

    var xs = x.tail
    for (i <- 0 until (x.size - 1)) {
      for (k <- 0 until states.size) {

        val (m, index) = (0 until states.size).map { p =>
          (gamma(p)(i) * transitions(p).prob(k), p)
        }.max

        psi(k)(i+1) = index
        gamma(k)(i+1) = m * emissions(k).prob(xs.head)
      }
      xs = xs.tail
    }

    var max = gamma(0)(x.size-1)
    var path = new Array[Int](x.size)
    path(x.size-1) = 0
    for (k <- 1 until states.size) {
      if (max < gamma(k)(x.size-1)) {
        max = gamma(k)(x.size-1)
        path(x.size-1) = k
      }
    }

    for (i <- (x.size-1) to 1 by -1) {
      path(i-1) = psi(path(i))(i)
    }
    (max, path.toList)
  }

  def forward(x: Stream[Int]) = {
    var alpha = Array.ofDim[LogProbability](states.size, x.size)

    for (k <- 0 until states.size) {
      alpha(k)(0) = initialProbabilities.prob(Stream(k)) * emissions(k).prob(x.head)
    }

    var xs = x.tail
    for (i <- 0 until (x.size - 1)) {
      for (k <- 0 until states.size) {
        alpha(k)(i+1) = alpha(0)(i) * transitions(0).prob(k)
        for (p <- 1 until states.size) {
          alpha(k)(i+1) += alpha(p)(i) * transitions(p).prob(k)
        }
        alpha(k)(i+1) *= emissions(k).prob(xs.head)
      }
      xs = xs.tail
    }

    var sum = alpha(0)(x.size - 1)
    for (k <- 1 until states.size) {
      sum += alpha(k)(x.size - 1)
    }

    (sum, alpha)
  }

  def backward(x: Stream[Int]) = {
    var beta = Array.ofDim[LogProbability](states.size, x.size)

    for (k <- 0 until states.size) {
      beta(k)(x.size - 1) = 100.0.%%;
    }

    var xs = x.reverse
    for (i <- (x.size - 2) to 0 by -1) {
      for (k <- 0 until states.size) {
        beta(k)(i) = transitions(k).prob(0) * emissions(0).prob(xs.head) * beta(0)(i+1)
        for (p <- 1 until states.size) {
          beta(k)(i) += transitions(k).prob(p) * emissions(p).prob(xs.head) * beta(p)(i+1)
        }
      }
      xs = xs.tail
    }

    var sum = 0.0.%%
    for (k <- 0 until states.size) {
      sum += beta(k)(0) * initialProbabilities.prob(Stream(k)) * emissions(k).prob(x.head)
    }

    (sum, beta)
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

    def hmm = new HiddenMarkovModel(emissionsMap, transitionsMap, initialProbabilitiesDistribution, symbols, states)
  }
}