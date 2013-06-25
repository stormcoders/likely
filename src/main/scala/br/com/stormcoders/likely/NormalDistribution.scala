package br.com.stormcoders.likely

import util.Random

class NormalDistribution(mean: Double, sd: Double) extends Distribution[Double] {
  def prob(x: Double) = 
    new LogProbability(((x-mean)*(x-mean)/(2*sd*sd)) + math.log(sd*math.sqrt(2*math.Pi)) )

  def choose : Double = {
    def createMember(x: Double) = mean + x*sd
    def boxMuller = {
      val uniform = new Random()
      val u1 = uniform.nextDouble
      val u2 = uniform.nextDouble
      val r = math.sqrt(-2*math.log(u1))
      math.sqrt(-2*math.log(u1))*math.cos(2*math.Pi*u2)
    }
    createMember(boxMuller)
  }
}

object NormalDistribution extends DistributionObject[Double] {
  def apply(mean: Double, sd: Double) =
    new NormalDistribution(mean, sd)
    
  def train(randomNumbers: Stream[Double]): NormalDistribution = {
    val mean = randomNumbers.foldLeft(0.0) { (a, b) => a + b/randomNumbers.length }
    val sumOfSquares = randomNumbers.foldLeft(0.0d){(a, b) => a + Math.pow(b - mean,2)}
    val sd = Math.sqrt(sumOfSquares/randomNumbers.length)
    new NormalDistribution(mean, sd)
  }
}