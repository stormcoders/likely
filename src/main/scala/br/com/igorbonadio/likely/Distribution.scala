package br.com.igorbonadio.likely

trait Distribution[T] {
  def prob(x: T): LogProbability
  def choose: T
}

trait DistributionObject[T] {
  def train(trainingSet: Stream[T]): Distribution[T]
}
