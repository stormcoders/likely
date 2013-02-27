class Alphabet(symbols: List[String]) {
  val alphabetMap = (symbols zip (0 to (symbols.length-1))).toMap
  
  def id(symbol: String):Int = alphabetMap get symbol match {
    case Some(x) => x
    case _       => -1
  }
}