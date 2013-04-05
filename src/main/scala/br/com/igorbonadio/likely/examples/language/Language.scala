package br.com.igorbonadio.likely.examples

import br.com.igorbonadio.likely._

object Language {

  def classify(text: String, debug: Boolean) = {
    // training set
    val pt = scala.io.Source.fromFile("src/main/scala/br/com/igorbonadio/likely/examples/language/pt.txt").
      mkString.toUpperCase.toList.filter(c => c.isLetter && c <= 'z').map(x => x.toString)
    val en = scala.io.Source.fromFile("src/main/scala/br/com/igorbonadio/likely/examples/language/en.txt").
      mkString.toUpperCase.toList.filter(c => c.isLetter && c <= 'z').map(x => x.toString)

    val alphabet = new Alphabet(List("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", 
                                     "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"))

    val ptModel = DiscreteIIDModel.train(alphabet.generateSequeceOfIds(pt.toStream))
    val enModel = DiscreteIIDModel.train(alphabet.generateSequeceOfIds(en.toStream))

    val test = text.
      toUpperCase.toList.filter(c => c.isLetter && c <= 'z').map(x => x.toString)

    val ptProb = ptModel.prob(alphabet.generateSequeceOfIds(test.toStream))
    val enProb = enModel.prob(alphabet.generateSequeceOfIds(test.toStream))
    
    if (debug) {
      print("ptProb = ")
      println(ptProb)
      print("enProb = ")
      println(enProb)
    }

    if (ptProb > enProb)
      "pt"
    else
      "en"
  }

  def main(args: Array[String]) {
    println("classification = " + classify(args(0), true))
  }
}