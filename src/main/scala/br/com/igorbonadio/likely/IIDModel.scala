package br.com.igorbonadio.likely

import util.Random

class IIDModel(distribution: DiscreteDistribution) {
  def prob(sequence: List[Int]):LogProbability = sequence match {
    case x::xs => distribution.prob(x) * prob(xs)
    case List() => Probability(1)
  }
  
  def choose = {
    def chooseWith(v: Double, prob: List[LogProbability], sym: Int): Int = prob match {
      case p::ps => if ((v - p.expValue) < 0) sym
              else chooseWith(v - p.expValue, ps, sym + 1)
      case List() => sym
    }
    chooseWith((new Random()).nextDouble, distribution.probabilities, 0)
  }
  
  def chooseSequence(size: Int): List[Int] = 
    if (size > 0) choose :: chooseSequence(size-1)
    else List()
}