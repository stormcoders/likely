package br.com.igorbonadio.likely

class NormalDistribution(mean: Double, sd: Double) {
  def prob(x: Double) = 
    new LogProbability(((x-mean)*(x-mean)/(2*sd*sd)) + math.log(sd*math.sqrt(2*math.Pi)) )
}