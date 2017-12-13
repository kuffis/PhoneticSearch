import org.scalatest.FunSuite

class PhoneticDistanceFilterSuite extends FunSuite {
  val segmenter = PronunSegmenterFactory.create()        
  val itemData = List[String]("a|b|a|t|o|r:/ə|ˈb|eɪ|t.|ɚ|/", "a|b|a|tt|oir:/ˈæ|b.|ə|ˌt|wɑɹ/", "a|b|a|t|u|r|e:/ˈæ|b.|ə.|t|jʊ|ɹ|/")
  val items = itemData.map(TrainingItemReader.parseTrainingItem).flatten
  val data = new TrainingData(items)

  test("it searches phonetically most similar words") {
    val filter = new PhoneticDistanceFilter(data, segmenter)
    val result = filter.filter("əb.əˌtwɑɹ", 2)
    assert(result(0)._2 < 0.2f) // Check if is close enough
    assert(result(0)._1.unsplitWord == "abattoir")
  }
  
  test("it concatenates smaller words to longer ones") {
    val filter = new PhoneticDistanceFilter(data, segmenter)
    val result = filter.filter("əb.əˌtwɑɹəb.əˌtwɑɹ", 2)
    assert(result(0)._2 < 0.6f) // Check if is close enough
    assert(result(0)._1.unsplitWord == "abattoir abattoir")
  }
}