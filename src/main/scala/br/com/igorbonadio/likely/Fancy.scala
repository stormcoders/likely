package br.com.igorbonadio.likely

import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

import Fancy._

object Fancy {
  implicit def DoubleToPercentage(value: Double) = new Percentage(value)

  class Percentage(value: Double) {
    def %% = Probability(value/100)
  }
}

class ProbabilityOf(alphabet: Alphabet) {
  var probs: ListBuffer[ProbabilityValueOf] = new ListBuffer()
  def distribution = {
    var dist: Array[LogProbability] = new Array[LogProbability](alphabet.size)
    probs.foreach { p =>
      dist(alphabet.id(p.name)) = p.value
    }
    new DiscreteDistribution(dist.toList)
  }
  def apply(symbol: String) = {
    probs.prepend(new ProbabilityValueOf(symbol))
    probs.head
  }
}

class ProbabilityValueOf(symbol: String) {
  var value:LogProbability = 0.0.%%
  def name = symbol
  def is(v: LogProbability) = {
    value = v
  }
}