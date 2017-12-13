import org.scalatest.FunSuite

class TrainingDataSuite extends FunSuite {

  test("it calculates utility data structures correctly") {
    val t1 = new TrainingItem(TrainingDataSuite.toSplitting("kissa"), "kissa", "KISSA", true)
    val t2 = new TrainingItem(TrainingDataSuite.toSplitting("koira", "KOIRÄ"), "koira", "KOIRA", true) 
    val t3 = new TrainingItem(null, "kana", "KANA", false)
    val data = new TrainingData(List[TrainingItem](t1, t2, t3))
    
    assert(data.phonemes == Set("K","I","S","A","O","R", "Ä"))
    assert(data.letters  == Set(data.START, data.END, "k","i","s","a","o","r"))
    assert(data.lettersToPhonemes("a").size == 2)
    assert(data.lettersToPhonemes("a").contains("A"))
    assert(data.lettersToPhonemes("a").contains("Ä"))
  }
  
  test("it can debug letter-to-phoneme splitting") {
    val t1 = new TrainingItem(TrainingDataSuite.toSplitting("kissa"), "kissa", "KISSA", true)
    t1.debugSplit()
  }
}

object TrainingDataSuite {
  def toSplitting(characters : String) : Array[(String, String)] = {
    toSplitting(characters, characters.toUpperCase)
  }
  def toSplitting(characters : String, phonemes : String) : Array[(String, String)] = {
    (characters.toList.map(a => ""+a) zip phonemes.toList.map(a => ""+a)).toArray
  }
}
