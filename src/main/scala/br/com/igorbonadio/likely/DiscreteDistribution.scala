package br.com.igorbonadio.likely

import util.Random

class DiscreteDistribution(probabilities: List[LogProbability]) extends Distribution[Int] {
  val probabilityMap = ((0 to (probabilities.length - 1)) zip probabilities).toMap
  
  def prob(id: Int):LogProbability = probabilityMap(id)

  def choose : Int = {
    def chooseWith(v: Double, prob: List[LogProbability], sym: Int): Int = prob match {
      case p::ps => if ((v - p.expValue) < 0) sym
              else chooseWith(v - p.expValue, ps, sym + 1)
      case List() => sym
    }
    chooseWith((new Random()).nextDouble, probabilities, 0)
  }
}

object DiscreteDistribution {
  def train(sequence: Stream[Int]): DiscreteDistribution = {
    new DiscreteDistribution(
      sequence.groupBy(x => x).toSeq.sortBy(_._1).map { case (k, v) => 
        Probability(v.length.toDouble/sequence.length) }.toList)
  }
}