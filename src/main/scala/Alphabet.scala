class Alphabet(symbols: List[String]) {
  val alphabetMap = (symbols zip (0 to (symbols.length-1))).toMap
  
  def size: Int = alphabetMap.size
  
  def id(symbol: String): Int = alphabetMap get symbol match {
    case Some(x) => x
    case _       => -1
  }
  
  def symbol(id: Int): String = 
    if (id >= symbols.length) ""
    else symbols(id)
    
  def generateSequeceOfSymbols(ids: List[Int]): List[String] =
    ids.map(id => symbol(id))
    
  def generateSequeceOfIds(symbols: List[String]): List[Int] =
    symbols.map(symbol => id(symbol))
}
