package br.com.igorbonadio.likely

import util.Random

abstract class IIDModel[T](distribution: Distribution[T]) {
  def prob(sequence: Stream[T]): LogProbability = sequence match {
    case x #:: xs => distribution.prob(x) * prob(xs)
    case Stream() => Probability(1)
  }

  def choose: Stream[T] = distribution.choose #:: choose
}

trait IIModelObject[T] {
  def train(trainingSet: Stream[T]): IIDModel[T]
}

class DiscreteIIDModel(distribution: DiscreteDistribution) extends IIDModel(distribution)
class ContinuousIIDModel(distribution: NormalDistribution) extends IIDModel(distribution)

object DiscreteIIDModel extends IIModelObject[Int] {
  def apply(probabilities: LogProbability*) =
    new DiscreteIIDModel(new DiscreteDistribution(probabilities.toList))

  def train(sequence: Stream[Int]): DiscreteIIDModel = 
    new DiscreteIIDModel(DiscreteDistribution.train(sequence))
}

object ContinuousIIDModel extends IIModelObject[Double] {
  def train(sequence: Stream[Double]): ContinuousIIDModel = 
    new ContinuousIIDModel(NormalDistribution.train(sequence))
}