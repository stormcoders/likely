package br.com.igorbonadio.likely.examples.language_classifier

import scala.io.Source

import br.com.igorbonadio.likely._

class LanguageClassifier {
  // training set
  val pt = justLetters(fileToString("src/main/scala/br/com/igorbonadio/likely/examples/language_classifier/pt.txt"))
  val en = justLetters(fileToString("src/main/scala/br/com/igorbonadio/likely/examples/language_classifier/en.txt"))
  val alphabet = Alphabet('A' to 'Z')
  val ptModel = DiscreteIIDModel.train(alphabet.generateSequeceOfIds(pt))
  val enModel = DiscreteIIDModel.train(alphabet.generateSequeceOfIds(en))
  val classifier = new BayesianClassifier(Map(
    "portuguese" -> ptModel,
    "english" -> enModel
  ))

  def classify(text: String) = {
    val test = justLetters(text)
    classifier.classify(alphabet.generateSequeceOfIds(test))
  }

  private def fileToString(filename: String) =
    Source.fromFile(filename).mkString

  private def justLetters(sequence: String) =
    sequence.toUpperCase.toList.filter(c => c.isLetter && c <= 'z').map(_.toString).toStream
}

object LanguageClassifier {
  def main(args: Array[String]) {
    val languageClassifier = new LanguageClassifier
    println("classification = " + languageClassifier.classify(args(0)))
  }
}