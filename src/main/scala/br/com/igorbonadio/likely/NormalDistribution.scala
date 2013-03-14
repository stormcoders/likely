package br.com.igorbonadio.likely

class NormalDistribution(mean: Double, sd: Double) extends Distribution[Double] {
  def createMember(x: Double) =
    mean + x*sd
    
  def prob(x: Double) = 
    new LogProbability(((x-mean)*(x-mean)/(2*sd*sd)) + math.log(sd*math.sqrt(2*math.Pi)) )
}